#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(visNetwork)
library(shiny)
library(igraph)
library(DT)
library(dplyr)
options(scipen = 999)

check_na<-function(x){if (is.na(x)==TRUE){return(0)}else{return(x)}}
#nodes <- read.csv("\\\\c\\s\\Node\\Data\\Location\\NEW_NODES_.csv", header=T, as.is=T)
#links <- read.csv("\\\\c\\s\\Edge\\Data\\Location\\NEW_EDGES_1.csv", header=T, as.is=T)
nodes <- read.csv("\\\\c\\s\\folder1\\Visualisation Tool\\NEW_NODES_.csv", header=T, as.is=T)
links <- read.csv("\\\\c\\s\\folder1\\Visualisation Tool\\NEW_EDGES_1.csv", header=T, as.is=T)

names(links)[names(links)=='CON_CUSTOMER_ID_NO']<-'from'
names(links)[names(links)=='SUB_CUSTOMER_ID_NO']<-'to'
nodes$group<-"NA"
nodes$group<-as.factor(as.character(nodes$SUR_NAME==""|nodes$NI_NO==""|is.na(nodes$SA_UTR_NO)))
levels(nodes$group)<-c(levels(nodes$group),"0")
nodes$group[nodes$id=="-1"]<-"0"
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
names(nodes)[names(nodes)=='CUSTOMER_ID_NO']<-'id'
cat(str(nodes))


server <- function(input, output) {
   
    
    subgv<-function(VN,gnet,thresh=0){
      withProgress(message = 'Running', value = 0, {
      n<-2 
      
      incProgress(1/n, detail = paste("making subgraph"))
      VN.graph<-subgraph(net,VN)
      l <- layout_with_fr(VN.graph)
      vndf_nodes=igraph::as_data_frame(VN.graph,what="vertices")
      vndf_edges=igraph::as_data_frame(VN.graph,what="edges")
      vis.nodes<- vndf_nodes
      vis.links_ <- vndf_edges
      vis.links<-vis.links_[which(vis.links_$SUM_of_TOTAL_PAYMENTS_SUM>thresh*1000),]
      thresh.vis.nodes<-c(unique(append(vis.links$from,vis.links$to)))
      vis.nodes<- vndf_nodes[match(thresh.vis.nodes,vndf_nodes$name),]
      cat(str(vndf_nodes))
      
      if (nrow(vis.links)+nrow(vis.nodes)>200){
      showModal(modalDialog(
        title = "Warning!",
        paste("Too many (",nrow(vis.links)+nrow(vis.nodes),") objects to render: Try increasing threshold."),
        size="s",
        easyClose = TRUE
      ))
        stop()
      }
      
      vis.nodes$id<-vis.nodes$name
      #vis.nodes$shape  <- "dot" 
      vis.nodes$font.size<-12
      vis.nodes$shadow <- TRUE # Nodes will drop shadow
      vis.nodes$title  <- paste(vis.nodes$P_EMP_NAME_1,vis.nodes$SUR_NAME,sep="~")# Text on click
      vis.nodes$label  <- paste(vis.nodes$P_EMP_NAME_1,vis.nodes$SUR_NAME, sep="~")# Node label
      vis.nodes$borderWidth <- 2 # Node border width
      vis.nodes$color.border <- "black"
      vis.nodes$color.highlight.background <- "orange"
      vis.nodes$color.highlight.border <- "darkred"
      vis.links$label<-format(vis.links$SUM_of_TOTAL_PAYMENTS_SUM,decimal.mark=".",big.mark=",")
      vis.links$font.size<-12
      vis.links$width <- 1 # line width
      vis.links$color <- c(unlist(lapply(lapply(vis.links$SUM_of_TOTAL_PAYMENTS_SUM/vis.links$ALL_PAYT_REC_CON,check_na),function(x){if(x>0.895){return("red")}else{return("grey")}})))   # line color  
      vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
      vis.links$smooth <- FALSE    # should the edges be curved?
      vis.links$shadow <- FALSE    # edge shadow
      s.vis.nodes<<-vis.nodes
      s.vis.links<<-vis.links
      vis.links$dashes<- c(unlist(lapply(vis.links$SUM_of_TOTAL_TAX_PAID_SUM,function(x){if(x==0){return(TRUE)}else{return(FALSE)}})))
      g.vis.nodes<<-vis.nodes%>%select("name",	"ALL_PAYT_MADE_CON","ALL_PAYT_REC_SUBCON",	"SUR_NAME",	"NI_NO",	"CUST_FACT_UTR_NO",	"SA_UTR_NO",	"COMPANY_REG_NO",	"TRADE_CLASS_IND",	"TRADE_NAME_1",	"TRADE_NAME_2",	"P_TRADE_NAME_1",	"P_TRADE_NAME_2",	"CORR_NAME_1",	"CORR_NAME_2",	"EMP_NAME_1",	"EMP_NAME_2",	"P_EMP_NAME_1",	"P_EMP_NAME_2",	"POST_CODE",	"id")
      g.vis.links<<-vis.links%>%select("from",	"to",	"SUM_of_MONTH_RETRNS_NO",	"SUM_of_ACTIVE_RETRNS_NO",	"SUM_of_TOTAL_PAYMENTS_SUM",	"SUM_of_TOTAL_TAX_PAID_SUM",	"ALL_PAYT_REC_CON",	"ALL_PAYT_MADE_CON",	"ALL_PAYT_REC_SUBCON",	"COUNT_of_SCHEME_ID_NO")
      output$table <-DT::renderDataTable(DT::datatable({g.vis.links},options = list(scrollX = TRUE)))
      output$table1 <-DT::renderDataTable(DT::datatable({g.vis.nodes},options = list(scrollX = TRUE)))
      incProgress(1/n, detail = paste("rendering"))
      })
     
      visnet <- visNetwork(vis.nodes, vis.links,main=as.character(isolate(input$n)),height = "2800px")%>%visIgraphLayout()%>%
        visGroups(groupname = "TRUE", shape = "icon",icon = list(code = "f1ad")) %>%
        visGroups(groupname = "FALSE", shape = "icon",icon = list(code = "f118",color="red")) %>%
        visGroups(groupname = "0", shape = "icon", 
                  icon = list(code = "f0c2", color = "grey",size=90))%>%addFontAwesome()%>%
        visPhysics(stabilization = FALSE)%>%visExport()
      
      
      visnet%>%visInteraction(navigationButtons = TRUE, selectable=TRUE)%>%visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
        visEvents(selectNode = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes);
                  ;}")%>%visHierarchicalLayout()
      
    }
    
    # get position info
    observeEvent(input$store_position, {
      visNetworkProxy("network") %>% visGetPositions()
    })
    
    # format positions
    nodes_positions <- reactive({
      positions <- input$network_positions
      if(!is.null(positions)){
        nodes_positions <- do.call("rbind", lapply(positions, function(x){ data.frame(x = x$x, y = x$y)}))
        nodes_positions$id <- names(positions)
        nodes_positions
      } else {
        NULL
      }
    })
    
    
    output$downloadNetwork <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        nodes_positions <- nodes_positions()
        if(!is.null(nodes_positions)){
          nodes_save <- merge(s.vis.nodes, nodes_positions, by = "id", all = T)
        } else  {
          nodes_save <- s.vis.nodes
        }
        
        visNetwork(nodes = nodes_save, edges = s.vis.links, height = "800px",width="100%") %>%
          visOptions(highlightNearest = TRUE) %>% visExport() %>%
          visGroups(groupname = "TRUE", shape = "icon",icon = list(code = "f1ad")) %>%
          visGroups(groupname = "FALSE", shape = "icon",icon = list(code = "f118",color="red")) %>%
          visGroups(groupname = "0", shape = "icon", 
                    icon = list(code = "f0c2", color = "grey",size=90))%>%addFontAwesome()%>%
          visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
      }
    )
    
    
    
    
    
    
    utr.get<-function(utr,depth=2,gnet){
      
      query<-c(strsplit(trimws(utr),"-")[[1]])
      
      
      if (length(query)>1){
        VN<-unlist(lapply(nodes$id[match(query,nodes$CUST_FACT_UTR_NO)],as.character))
        cat(VN)
        return(VN)
      }
      
      query_sp<-c(strsplit(trimws(utr),">")[[1]])
      
      if (length(query_sp)==2){
        
        sp_<-nodes$id[match(query_sp,nodes$CUST_FACT_UTR_NO)]
        sp<-shortest_paths(net, from = V(net)[name==sp_[1]], 
                           to  = V(net)[name==sp_[2]],
                           output = "both",mode = "all")
        VN<-names(V(net)[unlist(sp$vpath)])
        
        
        cat(VN)
        return(VN)
      }
      
      
      actor<-nodes[which(nodes$CUST_FACT_UTR_NO==as.double(utr)),]
      dist.from.actor<- distances(net, v=V(net)[name==as.numeric(actor['id'])], to=V(net), weights=NA)
      fdo<-dist.from.actor[1,is.finite(dist.from.actor)]
      fdo2<-fdo[fdo<depth]
      VN=c(names(fdo2)) 
      return(VN)
      
    }
    
 
    observeEvent({input$go
      input$thresh},
       {output$network<-renderVisNetwork({subgv(utr.get(isolate(input$n),gnet=net),net, thresh=isolate(input$thresh))})
    })
    
    
    myNode <- reactiveValues(selected = '')
    
    observeEvent(input$current_node_id, {
      myNode$selected <<- unlist(input$current_node_id["nodes"])
      visNetworkProxy("network") %>% visGetPositions()
      #cat(unlist(input$current_node_id["nodes"]))
    })
    
    observeEvent(input$param_nodeId, {
      str(input$param_nodeId)
      #cat(unlist(input$current_node_id["nodes"]))
    })
  
    output$table2<- DT::renderDataTable(DT::datatable({g.vis.nodes[g.vis.nodes$id==myNode$selected,]},options = list(searching = FALSE, paging= FALSE,scrollX = TRUE)))
    
}

ui <- navbarPage(
  #tags$head(tags$style(".shiny-notification {position: fixed; top: 60% ;left: 50%")),
  title = 'VIZ POC',
  tabPanel('Network Visualisation',     fluidRow( column(textInput(inputId = "n","Search By UTR", value = "5555555555"),width=2),
           column(actionButton("go","Go!"),width=1),column(sliderInput(inputId = "thresh",label="Threshold(K)",min=0,max=200,value=60,step=10),offset=4,width=5)),
           visNetworkOutput("network"),actionButton("store_position", "Freeze-frame"),downloadLink('downloadNetwork', 'Download network'),hr(),hr(),h4('Selected Contractor Details'),DT::dataTableOutput("table2")),
  tabPanel('Data',       h3("Transactions' Details"), DT::dataTableOutput("table"), hr(),h3("Contractors' Details"),DT::dataTableOutput("table1")),
  tabPanel('Help',h3("Query Types"),p("There a currently 3 query types available in this tool:"),br(),h4("1-deep Neigbourhood Search"),
           p("Enter a valid UTR into the search field and click Go!. A graphical representation of all contractors/subcontractors within
             1 hop of the supplied UTR(contractor) will be rendered. Triangles will also be represented"),code("1111111111"),br(),br(),h4("Group Search"),
            p("submit a list of valid UTRs delimited by -, click Go!. A graphical representation of how the contractors are linked will be rendered. See example"),code("1111111111-2222222222-3333333333"),
            br(),br(),h4("Shortest Path Search"),p("Enter any two valid UTRs delimited by >, click Go!. A graphical representation of the shortest unidirectional chain between both contractors will be rendered. See example"),code("1111111111>2222222222"),
           br(),br(),h3("Gotchas!"),p("Avoid introducing whitespaces in your search string."), p("Be mindful of the threshold filter as it could filter of all results!"))
  
  
  
)

shinyApp(ui = ui, server = server)

