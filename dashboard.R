# -----------------------------------------------------------------------------
dashboard_includeCSS <- function(filename) {
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = filename)
  )
}

# -----------------------------------------------------------------------------
dashboard_sourceModules <- function(path) {
  lapply(list.files(path = path, pattern = "*.R", full.names = TRUE), function(f) {
    source(f)
  })
}

# -----------------------------------------------------------------------------
dashboard_module <- function(module_id) {
  get(paste0(module_id, "_", "module"))
}

# -----------------------------------------------------------------------------
dashboard_call <- function(module_id, func) {
  func_obj <- paste0(module_id, "_", func)
  if (exists(func_obj)) {
    do.call(func_obj, args = list(id = module_id))
  }
  else {
    # do nothing - maybe give a warning
  }
}

# -----------------------------------------------------------------------------
dashboard_flattenModules <- function(modules) {
  do.call(c, lapply(modules, function(module_id) {
    module <- dashboard_module(module_id)
    if (is.null(module$subitems)) {
      module_id
    } else {
      c(module_id, module$subitems)
    }
  }))
}

# -----------------------------------------------------------------------------
dashboard_sidebarMenu <- function(modules) {
  sidebarMenu(
    id = "sidebar_menu",
    dashboard_menuItems(modules),
    hr(),
    dashboard_menuItemsConditional(modules)
  )
}

# -----------------------------------------------------------------------------
dashboard_menuItems <- function(modules, func = menuItem) {
  lapply(modules, function(module_id) {
    ns <- NS(module_id)

    module <- dashboard_module(module_id)
    if (is.null(module$dynamic) || module$dynamic == FALSE) {
      if (!is.null(module$subitems)) {
        subitems <- dashboard_menuItems(module$subitems, menuSubItem)
        func(text = module$name, icon = module$icon, tabName = module_id, subitems)
      } else {
        func(text = module$name, icon = module$icon, tabName = module_id)
      }
    } else {
      menuItemOutput(ns(module_id))
    }
  })
}

# -----------------------------------------------------------------------------
dashboard_menuItemsConditional <- function(modules) {
  lapply(dashboard_flattenModules(modules), function(module_id) {
    conditionalPanel(
      condition = sprintf("input.sidebar_menu == '%s'", module_id),
      dashboard_call(module_id, "sidebar")
    )
  })
}

# -----------------------------------------------------------------------------
dashboard_tabItems <- function(modules) {
  do.call(tabItems, lapply(dashboard_flattenModules(modules), function(module_id) {
    dashboard_call(module_id, "ui")
  }))
}

# -----------------------------------------------------------------------------
dastboard_setActiveTab <- function(session, id) {
  updateTabItems(session, "sidebar_menu", selected = id)
}

# -----------------------------------------------------------------------------
dashboard_server <- function(modules, input, output, session) {
  data <- NULL
  for (module_id in dashboard_flattenModules(modules)) {
    module <- dashboard_module(module_id)
    server_func <- paste0(module_id, "_", "server")
    if (exists(server_func)) {
      data <- callModule(function(input, output, session, id, data) {
        do.call(server_func, args = list(input, output, session, module_id, data))
      }, module_id, session = session, module_id, data)
    }
  }

  dastboard_setActiveTab(session, modules[1])
}


