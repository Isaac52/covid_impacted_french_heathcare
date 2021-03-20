info_card <- function(title, value,
                      main_icon = "total",
                      bg_color = "default", text_color = "default") {
    
    div(
        class = "panel panel-default",
        style = 'padding:0px;',
        div(
            class = str_glue("panel-body bg-default text-default"),
            p(class = "pull-right", icon(class = "fa-4x", main_icon)),
            h3(title),
            h5(value),
            
        )
    )
    
}
