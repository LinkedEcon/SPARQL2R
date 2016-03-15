library(SPARQL) # SPARQL querying package
library(igraph) # Plot package
library(vcd)
 
# Step 1 - Set up preliminaries and define query

# Define the LinkedEconomy.org endpoint

endpoint <- "http://143.233.226.49:8890/sparql"
 
# Create query statement

query <-
"PREFIX gr: <http://purl.org/goodrelations/v1#>
PREFIX dcTerms: <http://purl.org/dc/terms/>
SELECT (xsd:date(?date) as ?date) (max(?sellerLegalName) as ?sellerLegalName) (max(?buyerLegalName) as ?buyerLegalName) ?buyer (xsd:decimal(sum(?amount)) as ?sumAmount) FROM <http://linkedeconomy.org/DiavgeiaII/2015>
FROM <http://linkedeconomy.org/Organizations>
WHERE {
?spendingItem elod:hasExpenditureLine ?expenditureLine ;
                 elod:buyer ?buyer ;
                 rdf:type elod:ExpenseApprovalItem ;
                 dcTerms:issued ?date .
?expenditureLine elod:amount ?ups ;
                 elod:seller ?seller .
?ups gr:hasCurrencyValue ?amount .
OPTIONAL {?buyer gr:legalName ?buyerLegalName} .
OPTIONAL {?seller gr:legalName ?sellerLegalName} .
** #FILTER (?date > 1420063200) . **
#FILTER (?date < 1483221600) .
FILTER NOT EXISTS {?spendingItem elod:hasCorrectedDecision ?correctedDecision} .
#FILTER (?decisionTypeId = xsd::String(Β.2.2) ) .
FILTER (?seller=<http://linkedeconomy.org/resource/Organization/094019245>)
}
GROUP BY ?date ?buyer ?buyerLegalName ?sellerLegalName
ORDER BY DESC (?sumAmount)
LIMIT 10"

#---#

# Step 2 - Use SPARQL package to submit query and save results to a data frame

Sys.setlocale(category = "LC_ALL", locale = "greek")
query_data <- SPARQL(endpoint,query)
#query_data$results <- iconv(query_data$results, "UTF-8")
print(query_data$results)
#summary(query_data)
queryResult <- query_data$results
#str(queryResult)
s <- summary(queryResult)

#---#
 
# Step 3 - Create graph data frame and Print some statistics.

# Load (DIRECTED) graph from data frame 
sparql_graph <- graph.data.frame(query_data$results, directed=TRUE)

# Number of the vertices-size of the graph
Vertices <- vcount(sparql_graph)
#cat("Vertices: ", Vertices, "\n")

# Number of the edges-order of the graph
Edges <- ecount(sparql_graph)
#cat("Edges: ", Edges, "\n")

# Density of the graph
Density <- graph.density(sparql_graph)
#cat("Density: ", Density, "\n")

#Diameter of the graph
Diameter <- diameter(sparql_graph, directed = TRUE, unconnected = TRUE, weights = NULL)
#cat("Diameter: ", Diameter, "\n")

# Vertex Connectivity Number
# The minimum number of nodes whose deletion from a graph disconnects it.
Vertex_Connectivity <- vertex.connectivity(sparql_graph)
#cat("Vertex Connectivity: ", vertex.connectivity(sparql_graph), "\n")

# Edge Connectivity Number
# The minimum number of edges whose deletion from a graph disconnects it.
Edge_Connectivity <- edge.connectivity(sparql_graph)
#cat("Edge Connectivity: ", edge.connectivity(sparql_graph), "\n")

# The degree distribution
ddg <- degree(sparql_graph)
#cat("The summary of the degree distribution: \n")
#print(summary(ddg))

# Standard Deviation
Standard_Deviation <- sd(ddg)
#cat("Standard Deviation: ", sd(ddg), "\n")
             
# Coefficient of variation
CoefficientOfVariation <- sd(ddg)/mean(ddg)
#cat("Coefficient of variation: ", CoefficientOfVariation, "\n")

# Average path length
AveragePathLength <- average.path.length(sparql_graph, directed=TRUE, unconnected=TRUE)

# Make a data frame with degree distribution
tg <- table(ddg)           
vertp <- as.integer(names(tg))    
## Names are the number of vertices
A <- data.frame("degrees" = vertp, "number of vertices" = as.vector(tg))
#cat("Frequency distribution: \n")
#print(A) ## frequency distribution

## plot the degree distribution 
#B <- degree.distribution(sparql_graph, cumulative=FALSE)   
#par(mfrow=c(1,2))
#plot(B, xlab="degree", ylab="freq");
#hist(ddg , freq=NULL, xlab="degree", ylab="freq")

write.table(rbind(Vertices, Edges, Density, Diameter, Vertex_Connectivity, Edge_Connectivity,
Standard_Deviation, CoefficientOfVariation, AveragePathLength), 
"MyData.csv", sep = ",", col.names = F)

#---#

# Step 4 - Plotting the graph #

sparql_graph <- simplify(sparql_graph, remove.multiple = F, remove.loops = T)

plot.igraph(sparql_graph, edge.arrow.size=.2, edge.color="grey35", edge.curved=.1, edge.arrow.width=E(sparql_graph)$weight, 
vertex.size=6, vertex.frame.color="orange", vertex.color="grey85", vertex.label=NA)
#vertex.label.color="tomato", vertex.label.font=1, vertex.label.cex=0.8)
