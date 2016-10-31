# -----------------------------------------------------------------------------
dashboard_sourceModules <- function(modules) {
  lapply(modules, function(module_id) {
    source(file.path("modules", paste0(module_id, ".R")))
  })
}

# -----------------------------------------------------------------------------
dashboard_moduleMethod <- function(module_id, method) {
  method_obj <- paste0(module_id, "_", method)
  if (exists(method_obj))
    do.call(method_obj, args = list(id = module_id))
  else {
    # do nothing - maybe give a warning
  }
}

# -----------------------------------------------------------------------------
dashboard_menuItems <- function(modules) {
  lapply(modules, function(module_id) {
    module <- dashboard_moduleMethod(module_id, "module")
    menuItem(text = module$name, icon = module$icon, tabName = module_id)
  })
}

# -----------------------------------------------------------------------------
dashboard_menuItemsConditional <- function(modules) {
  lapply(modules, function(module_id) {
    module <- dashboard_moduleMethod(module_id, "module")
    conditionalPanel(
      condition = sprintf("input.sidebar_menu == '%s'", module_id),
      hr(),
      dashboard_moduleMethod(module_id, "sidebar")
    )
  })
}

# -----------------------------------------------------------------------------
dashboard_tabItems <- function(modules) {
  do.call(tabItems, lapply(modules, function(module_id) {
    dashboard_moduleMethod(module_id, "ui")
  }))
}

# -----------------------------------------------------------------------------
dashboard_server <- function(modules, input, output, session) {
  lapply(modules, function(module_id) {
    method_obj <- paste0(module_id, "_", "server")
    if (exists(method_obj))
      do.call(method_obj, args = list(id = module_id, input, output, session))
  })
}


