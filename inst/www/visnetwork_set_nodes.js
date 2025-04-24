Shiny.addCustomMessageHandler("set_selected_nodes", function(nodes) {
    Shiny.setInputValue("visNetwork_selected_nodes", nodes, {priority: "event"});
});

Shiny.addCustomMessageHandler("set_selected_nodes_subgraph", function(nodes) {
    Shiny.setInputValue("visNetwork_selected_nodes_subgraph", nodes, {priority: "event"});
});