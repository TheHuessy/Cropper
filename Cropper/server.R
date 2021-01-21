##### LIBRARY #####

library(shiny)
library(magick)
library(shinyjs)
library(httr)
library(DBI)
library(RPostgreSQL)
library(yaml)
library(tools)



##### SERVER DEFINITION #####

shinyServer(function(input, output, session) {

  ## NOTE: Turns out this all loads a LOT faster if all the defs are inside the shinyServer function. 
  ##### FUNCTION DEFS #########

  ## Creds and SQL Driver Establishment

  creds <<- read_yaml(Sys.getenv("CREDS_PATH"))


  sql_connect <- function(creds){
    dbConnect(dbDriver("PostgreSQL"),
              host=as.character(creds$pg_host),
              user=as.character(creds$pg_user),
              password=as.character(creds$pg_pw),
              dbname="strobot"
    )
  }


  pull_data <- function(){
    sql_con <- sql_connect(creds)
    pull_data <- dbGetQuery(sql_con,statement="SELECT link_id, end_link as URL, processed, 'pulls' as table_name FROM pulls WHERE processed is NULL AND link_type = 'Direct' ORDER BY random() LIMIT 50")
    ext_data <- dbGetQuery(sql_con,statement="SELECT link_id, piece as URL, processed, 'culling_external' as table_name FROM culling_external WHERE keep = 1 AND processed is NULL ORDER BY random() LIMIT 50")
    dir_data <- dbGetQuery(sql_con,statement="SELECT link_id, end_link as URL, processed, 'culling_direct' as table_name FROM culling_direct WHERE keep = 1 AND processed is NULL ORDER BY random() LIMIT 50")

    work <- rbind(pull_data, ext_data, dir_data) %>%
      .[sample(nrow(.)),]
    dbDisconnect(sql_con)
    return(work)
  }


  pull_total <- function(){
    sql_con <- sql_connect(creds)
    tot_left <- dbGetQuery(sql_con, statement="SELECT SUM(total) FROM (SELECT COUNT(*) as total FROM culling_external WHERE keep = 1 AND processed is NULL UNION ALL SELECT COUNT(*) as total FROM culling_direct WHERE keep = 1 AND processed is NULL UNION ALL SELECT COUNT(*) as total FROM pulls WHERE processed is NULL AND link_type = 'Direct') as tbl") %>%
      format(big.mark = ",")
    dbDisconnect(sql_con)
    return(tot_left)
  }

  # Pull insta links
  insta_fresh <- function(piece){
    pre_url <- paste("https://www.instagram.com",piece, "media/?size=l", sep="")
    fresh_url = GET(pre_url)
    return(fresh_url$url)
  }


  #Parse insta and non-insta links
  get_link <- function(cnt){
    test_link <- corp$url[cnt]
    test <- grep(pattern="/p/", x=test_link)
    if (length(test) == 0){
      output_link <- corp$url[cnt]
    } else {
      output_link <- insta_fresh(corp$url[cnt])
    }

    return(output_link)
  }

  back_cnt_safe <- function(){

    if (length(idx_chain) > 0){
      new_cnt <<- idx_chain[length(idx_chain)]
      idx_chain <<- idx_chain[-new_cnt]

      return(new_cnt)
    } else {
      return(cnt)
    }
  }

  get_cnt_safe <- function(work, cnt){
    new_cnt <<- cnt + 1
    if (new_cnt > nrow(work)){
      print("reached the end of this batch, pulling more...")
      update_process_flag()
      corp <<- pull_data()

      new_cnt <<- 1
      idx_chain <<- c()
      ## Probably pull data here and update tots instead of just returning new_cnt
      ## Also will need to later on write in logic that allows this to verify that cnt 1 isn't going to fail
      ## as it is, we assume that #1 from the next batch pulled will work regardless
      ## Not highly likey, but still wise to prepare for
      return(new_cnt)
    }

    while (TRUE){
      next_ext_bool <<- file_ext(work$url[new_cnt]) %in% c("mp4", "mkv", "gif", "avi", "m4v", "m4p", "mpg")
      if (next_ext_bool == TRUE){
        # Advance one more
        new_cnt <<- new_cnt + 1
      } else {

        tester <<- get_link(new_cnt)

        url_check <<- GET(tester)

        if (url_check$status_code == 200){
          break
        } else {
          new_cnt <<- new_cnt + 1
        }
      }
    }
    return(new_cnt)
  }

  image_parse <- function(link, flip){

    img_dat_raw <- image_read(link) %>% 
      image_rotate(flip)

    img_dat_resize <- img_dat_raw %>% 
      image_scale(., "x720")

    #get original size info
    resize_info <- image_info(img_dat_resize)
    #get the width
    resize_width <- resize_info$width
    #get the height
    resize_height <- resize_info$height

    #get original size info
    raw_info <- image_info(img_dat_raw)
    #get the width
    raw_width <- raw_info$width
    #get the height
    raw_height <- raw_info$height

    im_dat <- list(link=link,
                   raw_img=img_dat_raw,
                   resize_img=img_dat_resize,
                   resize_info=resize_info,
                   resize_width=resize_width,
                   resize_height=resize_height,
                   raw_info=raw_info,
                   raw_width=raw_width,
                   raw_height=raw_height)
    return(im_dat)

  }



  fetch_image_data <- function(cnt){
    im_dat_link <<- as.character(get_link(cnt))
    im_dat <<- image_parse(im_dat_link, rotate_degree)
    return(im_dat)
  }

  fetch_placeholder <- function(){
    im_dat_link <<- "https://i.ytimg.com/vi/0FHEeG_uq5Y/maxresdefault.jpg"
    im_dat <<- image_parse(im_dat_link, rotate_degree)
    return(im_dat)
  }

  initial_view <- function(){
    output$SlogOutput <- renderUI({
      actionButton(inputId = 'START',
                   label = 'START')
    })
  }

  get_bounds <- function(brush_info){
    display_width <- im$resize_width
    display_height <- im$resize_height

    original_width <- im$raw_width
    original_height <- im$raw_width

    x_conversion <- display_width/original_width
    y_conversion <- display_height/original_height

    yx <- brush_info$ymax
    yn <- brush_info$ymin
    xx <- brush_info$xmax
    xn <- brush_info$xmin

    #[X.size.total width]x[Y.size.total height]+[X.offset from the left]+[Y.offset from the top]
    #[Xs]x[Ys]+[Xo]+[Yo]

    Xs <- (xx-xn)*x_conversion
    Ys <- (yx-yn)*y_conversion
    Xo <- xn*x_conversion
    Yo <- yn*y_conversion


    boxc <- paste(Xs, "x", Ys, "+", Xo, "+", Yo, sep = "")

    return(boxc)
  }

  clear_sections <- function(){
    section_o <<- FALSE
    section_o_click <<- 0

    section_w <<-FALSE
    section_w_click <<- 0

    section_i <<- FALSE
    section_i_click <<- 0

    section_a <<- FALSE
    section_a_click <<- 0
  }

  refresh_image <- function(im){

    output$Main <- renderImage({
      resize_width <- im$resize_width
      resize_height <- im$resize_height

      #Writing the display image as a temp file to be displayed
      te <- im$resize_img %>%
        image_write(tempfile(fileext = 'jpg'), format = 'jpg')
      list(src = te, width = resize_width, height = resize_height, contentType = "image/jpeg")
    }, deleteFile = TRUE)
  }

  update_text_outputs <- function(){
    output$Tots <- renderPrint({
      done_so_far <- reactive({crops_done})#crops in corpus
      cat(done_so_far())
    })
    output$ImLeft <- renderPrint({ #images left
      ifl <- pull_total()$sum
      cat(ifl)
    })
    output$ImDone <- renderPrint({ #Images done
      done <- reactive({images_done})
      cat(done())
    })
    output$crops_not_saved <- renderPrint({ #crops this session
      cat(unsaved_crops)
    })
  }

  get_sections <- function(){
    output_list <<- c()

    if (section_o){
      output_list <<- c(output_list, "o")
    }
    if (section_w){
      output_list <<- c(output_list, "w")
    }
    if (section_i){
      output_list <<- c(output_list, "i")
    }
    if (section_a){
      output_list <<- c(output_list, "a")
    }

    return(paste(output_list,collapse = "|"))
  }

  proc_flag_flip <- function(){
    if (is.na(corp$processed[cnt]) == TRUE){
      corp$processed[cnt] <<- TRUE
    }
  }

  undo_last_df_entry <- function(){
    crop_output_df <<- crop_output_df[-nrow(crop_output_df),]
  }

  reset_output_df <- function(){
    crop_output_df <<- data.frame()
  }

  write_df_to_sql <- function(df){

    sql_con <- sql_connect(creds)

    for (idx in 1:nrow(df)){
      link_info <- df$link_id[idx]
      url_info <- df$url[idx]
      section_info <- df$section_tag[idx]
      rotate_info <- df$rotate_degree[idx]
      crop_info <- df$crop_string[idx]

      insert_string <- paste("INSERT INTO cropped(link_id, url, section_tag, rotate_degree_string, crop_string) VALUES(",
                             "'",link_info, "'", ",",
                             "'",url_info, "'", ",",
                             "'",section_info, "'", ",",
                             rotate_info, ",",
                             "'",crop_info, "'",
                             ")",
                             sep = ""
      )
      dbExecute(sql_con, insert_string)

      # Keeping this in for logging purposes, may end up being too much in the long run
      print(insert_string)
    }
    dbDisconnect(sql_con)
  }

  check_sections <- function(){
    if (section_o == FALSE & section_w == FALSE & section_i == FALSE & section_a == FALSE){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }


  update_process_flag <- function(){

    sql_con <- sql_connect(creds)

    updated_links <- corp[which(corp$processed == TRUE),]

    if (nrow(updated_links) > 0){

      for (idx in 1:nrow(updated_links)){
        link_id_info <- updated_links$link_id[idx]
        url_info  <- updated_links$url[idx]
        tbl_name <- updated_links$table_name[idx]

        if (tbl_name == "culling_external"){
          col_name <- "piece"
        } else {
          col_name <- "end_link"
        }


        update_string <- paste("UPDATE ",
                               tbl_name,
                               " SET processed = 'TRUE'",
                               " WHERE link_id = '",
                               link_id_info,
                               "' AND ",
                               col_name,
                               " = '",
                               url_info,
                               "'",
                               sep = ""
        )
        # For logging
        print(update_string)

        dbExecute(sql_con, update_string)
      }
    }
    dbDisconnect(sql_con)

    #Not needed when only used on exit, but this will allow for future corp flushing if need be/auto save functionality
    corp <<- corp[-which(corp$processed == TRUE),]
  }

  save_crops <- function(){
    write_df_to_sql(crop_output_df)
    unsaved_crops <<- 0
    reset_output_df()
    update_text_outputs()
  }

  ##### STARTUP LOG PRINTING #####
  print(format(Sys.time(), "%Y-%m-%d %H:%M"))

  ##### ON STOP PARAMETERS #####

  onStop(function(){
           # Put this in the log so we know where a session ended
           update_process_flag()
           if (exists("sql_con")){
                 dbDisconnect(sql_con)
                   }
    print("========================")
  })

  ##### VARIABLE ESTABLISHMENT #####


  corp <<- pull_data()
  total_crops_left <<- pull_total()
  reset_output_df()


  crops_done <<- 0
  images_done <<- 0
  unsaved_crops <<- 0
  crops_left <<- nrow(corp[which(is.na(corp$processed)),])

  rotate_degree <<- 0
  crop_string <<- ""
  clear_sections()

  idx_chain <<- c()


  cnt <<- get_cnt_safe(corp,0)

  im <<- fetch_image_data(cnt)

  update_text_outputs()


  ################# INITIAL LOADOUT VIEW ####################

  initial_view()

  #################   REVEAL WORK AREA   ####################
  observeEvent(input$START,{
    output$SlogOutput <- renderUI({
      div(
        fluidRow(
          column(width = 8,
                 imageOutput("Main",
                             click = "plot_click1",
                             brush = brushOpts(
                               id = "plot_brush1",
                               resetOnNew = TRUE
                             ),
                             height = 720,
                             width = 1280
                 )
          ),
          column(
            width = 2,
            actionButton(
              inputId = "section_head",
              label = "O"
            ),
            br(),
            actionButton(
              inputId = "section_chest",
              label = "W"
            ),
            br(),
            actionButton(
              inputId = "section_bauch",
              label = "I"
            ),
            br(),
            actionButton(
              inputId = "section_end",
              label = "A"
            ),
            br(),
            br(),
            h3("Images Left"),
            textOutput("ImLeft",
                       inline = TRUE
            ),
            h3("Images Done"),
            textOutput("ImDone",
                       inline = TRUE),
            h3("Crops This Session"),
            textOutput("Tots",
                       inline = TRUE
            ),
             h3("Crops Not Saved"),
            textOutput("crops_not_saved",
                       inline = TRUE
            )
           ),
          column(
            width = 2,
            actionButton(
              inputId = "Crop",
              label = "Capture Selection"
            ),
            br(),
            actionButton(
              inputId = "Next",
              label = "Next Image"
            ),
            br(),
            actionButton(
              inputId = "Rotate",
              label = "Rotate"
            ),
            br(),
            actionButton(
              inputId = "Last",
              label = "Previous Image"
            ),
            br(),
            actionButton(
              inputId = "Undo",
              label = "Undo Last"
            ),
            br(),
            actionButton(
              "save", 
              "Save All Crops"
            ),
           br(),
            actionButton(
              "hide", 
              "Hide"
            )

          )

        )
      )
    })
  })

  #################        THE OUTPUT       #####################


  ##### MAIN IMAGE OUTPUT #####   

  refresh_image(im)

  ##### HIDE BUTTON #####
  observeEvent(input$hide, {
                 initial_view()
  })

  ##### O BUTTION #####
  observeEvent(input$section_head, {
    section_o_click <<- section_o_click + 1 

    if (section_o_click %% 2 == 0){
      section_o <<- FALSE
    } else {
      section_o <<- TRUE
    }

  })

  ##### W BUTTON #####
  observeEvent(input$section_chest, {
    section_w_click <<- section_w_click + 1 
    if (section_w_click %% 2 == 0){
      section_w <<- FALSE
    } else {
      section_w <<- TRUE
    }
  })

  ##### I BUTTON #####
  observeEvent(input$section_bauch, {
    section_i_click <<- section_i_click + 1 
    if (section_i_click %% 2 == 0){
      section_i <<- FALSE
    } else {
      section_i <<- TRUE
    }
  })

  ##### A BUTTON #####
  observeEvent(input$section_end, {
    section_a_click <<- section_a_click + 1 
    if (section_a_click %% 2 == 0){
      section_a <<- FALSE
    } else {
      section_a <<- TRUE
    }
  })

  ##### CROP BUTTON #####
  observeEvent(input$Crop, {
    if (is.null(input$plot_brush1) == TRUE ){
      showModal(
        modalDialog(
          title = "Awww Snap!",
          "Can't capture nothing. You need to make a selection by selecting an area of the picture",
          footer = modalButton("My B"),
          easyClose = TRUE
        )
      )
    } else if (!check_sections()){
      showModal(
        modalDialog(
          title = "Awww Snap!",
          "You need to choose a section button",
          footer = modalButton("My B"),
          easyClose = TRUE
        )
      )
      } else {

      bounds <<- get_bounds(input$plot_brush1)

      section_string <<- get_sections()

      crops_done <<- crops_done + 1

      unsaved_crops <<- unsaved_crops + 1

      #link_id|url/peice|section_tag|rotate_degree_string[optional]|crop_string|

      new_line <<- data.frame(link_id=corp$link_id[cnt],
                              url=corp$url[cnt],
                              section_tag=get_sections(),
                              rotate_degree=rotate_degree,
                              crop_string=bounds
                              )



      crop_output_df <<- rbind(crop_output_df, new_line)  

      if (crops_done %% 5 == 0){
        save_crops()
      }

      clear_sections()

      update_text_outputs()

    }
  })
  ##### ROTATE BUTTON #####

  observeEvent(input$Rotate, {

    rotate_degree <<- rotate_degree + 90

    im <<- fetch_image_data(cnt)

    refresh_image(im)

  })


  ##### UNDO BUTTON #####

  ## CHECK TO MAKE SURE UNDO IS WHAT YOU WANT
  observeEvent(input$Undo, {
    showModal(
      modalDialog(
        title = "Undo?",
        "Do you want to undo the last crop?",
        footer = tagList(
          actionButton("RealUndo", "Yes"),
          modalButton("Nah")
        )
      )
    )
  })

  ## PERFORM UNDO ACTION
  observeEvent(input$RealUndo,{
    crop_df_length <<- nrow(crop_output_df)
    if (crop_df_length < 1){
      showModal(
        modalDialog(
          title = "Undo?",
          "There was nothing to undo",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Word")
          )
        )
      )
    } else{
      crops_done <<- crops_done - 1
      if (unsaved_crops > 0){
        unsaved_crops <<- unsaved_crops - 1
      }

      undo_last_df_entry()
      update_text_outputs()

      showModal(
        modalDialog(
          title = "Undone!",
          "The last crop was removed!",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Word")
          )
        )
      )
    }
  })

  ##### NEXT BUTTON #####

  observeEvent(input$Next, {

    images_done <<- images_done + 1
    proc_flag_flip()
    idx_chain <<- c(idx_chain, cnt)

    rotate_degree <<- 0

    cnt <<- get_cnt_safe(corp, cnt)

    update_text_outputs()    

    if (cnt > length(corp$url)) {
      im <<- fetch_placeholder()
    } else {
      im <<- fetch_image_data(cnt)
    }
      refresh_image(im)
  })


  ##### PREVIOUS BUTTON #####

  observeEvent(input$Last, {

    cnt <<- back_cnt_safe()
    rotate_degree <<- 0
    images_done <<- images_done - 1

    update_text_outputs()

    if (cnt < 0) {
      im <<- fetch_placeholder()

      refresh_image(im)

    } else {
      im <<- fetch_image_data(cnt)

      refresh_image(im)

    }
  })


  ##### SAVE BUTTON #####
  observeEvent(input$save, {
    save_crops()
    showModal(
      modalDialog(
        title = "Save Complete",
        "Your data has been saved!",
        footer = modalButton("Hell Yeah"),
        easyClose = TRUE
      )
    )
  })

})

