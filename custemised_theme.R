my_theme <- function(data, font_name, font_size) {
  font_name = as.character(font_name)
  font_size = as.character(font_size)
  c_col = c("#274060", "#2f5375", "#4073a0", "#5088b9")
  tab_options(
    data = data,
    table.font.name = font_name,
    table.font.size = font_size,
    table.font.color = c_col[1],
    
    # adding border to top side of the table
    # table.border.top.style = "solid",
    # table.border.top.color = c_col[1],
    # table.border.top.width = px(2),
    
    # adding border to bottom side of the table
    # table.border.bottom.style = "solid",
    # table.border.bottom.color = c_col[1],
    # table.border.bottom.width = px(2),
    
    # adding border to right side of the table
    # table.border.right.style = "solid",
    # table.border.right.color = c_col[1],
    # table.border.right.width = px(2),
    
    # adding border to left side of the table
    # table.border.left.style = "solid",
    # table.border.left.color = c_col[1],
    # table.border.left.width = px(2),
    
    # adding border to the bottom of the table body
    # table_body.border.bottom.style = "solid",
    # table_body.border.bottom.color = c_col[1],
    # table_body.border.bottom.width = px(2),
    
    # column_labels.border.top.color = "black",
    # column_labels.border.top.width = px(2),
    
    # column_labels.border.bottom.color = c_col[2],
    # column_labels.border.bottom.width = px(2),
    
    # column_labels.border.lr.style = "solid",
    # column_labels.border.lr.width = px(1),
    # column_labels.border.lr.color = "black",
    
    # table_body.hlines.style = "solid",
    # table_body.hlines.width = px(1),
    # table_body.hlines.color = "black",
    
    table.background.color = "white",
    
    heading.align = "center",
    heading.background.color = "white",
    
    column_labels.background.color = "white",
    # column_labels.vlines.color = "black",
    # column_labels.vlines.style = "solid",
    # column_labels.vlines.width = px(1)
    
    # data_row.padding = px(10)
  )  %>% 
    
    cols_align(align = "center",columns = everything()) %>%
    
    #add border to the header
    # tab_style(style = cell_borders(sides = "all", weight = px(2)),
    #           locations = cells_title("title")) %>%
    
    # centered the stub heads
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    
    tab_options(
      column_labels.border.top.color = "#999999",
      column_labels.border.bottom.color = "#999999",
      table_body.border.bottom.color = "#999999",
      table_body.hlines.color = "white"
    )
  
}