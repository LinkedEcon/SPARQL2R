library(SPARQL) # SPARQL querying package
library(igraph) # Plot package
library(vcd)
 
# Step 1 - Set up preliminaries and define query

# Define the LinkedEconomy.org endpoint

endpoint <- "http://143.233.226.49:8890/sparql"
 
# Create query statement

query <-
"PREFIX elod: <http://linkedeconomy.org/ontology#>
PREFIX dcTerms: <http://purl.org/dc/terms/>
PREFIX gr: <http://purl.org/goodrelations/v1#>

SELECT DISTINCT (xsd:string(?094019245) as ?OTE) (STR(?vatId) AS ?vatId) (xsd:decimal(?amount) AS ?spendingItemAmount)
FROM <http://linkedeconomy.org/DiavgeiaII/2015>
FROM <http://linkedeconomy.org/Organizations>
WHERE {
?spendingItem elod:hasExpenditureLine ?expLine ;
              elod:buyer ?buyer ;
              dcTerms:issued ?date ;
              rdf:type elod:ExpenseApprovalItem .
?expLine elod:amount ?ups ;
         elod:seller <http://linkedeconomy.org/resource/Organization/094019245> .
?ups gr:hasCurrencyValue ?amount .
?buyer gr:vatID ?vatId .
}
OFFSET 100
LIMIT 2000"

#---#

# Step 2 - Use SPARQL package to submit query and save results to a data frame

query_data <- SPARQL(endpoint,query)
#print(query_data$results[3])
#typeof(query_data)
#summary(query_data)
queryResult <- query_data$results
#str(queryResult)
s <- summary(queryResult)

#---#
 
# Step 3 - Create graph data frame, Print some statistics and Plot a graph.

# Load (DIRECTED) graph from data frame 
sparql_graph <- graph.data.frame(queryResult, directed=TRUE)

# Number of the vertices-size of the graph
cat("Vertices: ", vcount(sparql_graph), "\n")

# Number of the edges-order of the graph
cat("Edges: ", ecount(sparql_graph), "\n")

# Density of the graph
cat("Density: ", graph.density(sparql_graph), "\n")

# Vertex Connectivity Number
# The minimum number of nodes whose deletion from a graph disconnects it.
cat("Vertex Connectivity: ", vertex.connectivity(sparql_graph), "\n")

# Edge Connectivity Number
# The minimum number of edges whose deletion from a graph disconnects it.
cat("Edge Connectivity: ", edge.connectivity(sparql_graph), "\n")

# The degree distribution
ddg <- degree(sparql_graph)
cat("The summary of the degree distribution: \n")
print(summary(ddg))

# Standard Deviation
cat("Standard Deviation: ", sd(ddg), "\n")
             
# Coefficient of variation
cvddg <- sd(ddg)/mean(ddg)
cat("Coefficient of variation: ", cvddg, "\n")

# Make a data frame with degree distribution
tg <- table(ddg)           
vertp <- as.integer(names(tg))    
## Names are the number of vertices
A <- data.frame("degrees" = vertp, "number of vertices" = as.vector(tg))
cat("Frequency distribution: \n")
print(A) ## frequency distribution

## plot the degree distribution 
#B <- degree.distribution(sparql_graph, cumulative=FALSE)   
#par(mfrow=c(1,2))
#plot(B, xlab="degree", ylab="freq");
#hist(ddg , freq=NULL, xlab="degree", ylab="freq")

#---#

# Plotting the graph #
sparql_graph <- simplify(sparql_graph, remove.multiple = F, remove.loops = F)

#layout <- layout.fruchterman.reingold(sparql_graph, niter=10000)

plot.igraph(sparql_graph, layout=layout.kamada.kawai, edge.arrow.size=0.5, edge.color="grey35", edge.curved=.1, 
edge.width=E(sparql_graph)$spendingItemAmount/100000, vertex.size=6, vertex.frame.color="orange", 
vertex.color="grey85", vertex.label=NA)
