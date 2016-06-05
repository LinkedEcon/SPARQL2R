library(SPARQL)
library(igraph)

endpoint <- "http://143.233.226.49:8890/sparql"

query <-
"PREFIX elod: <http://linkedeconomy.org/ontology#>
PREFIX dcTerms: <http://purl.org/dc/terms/>
PREFIX gr: <http://purl.org/goodrelations/v1#>

SELECT DISTINCT (xsd:string(094019245) AS ?OTEvatId) (STR(?vatId) AS ?vatId) (xsd:decimal(?amount) AS ?spendingItemAmount) 
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
ORDER BY DESC(?spendingItemAmount)
LIMIT 50"

query_data <- SPARQL(endpoint,query)
queryResult <- query_data$results

print (query_data$results)

G <- graph.data.frame(queryResult , directed=F)

#V(G)$color<-ifelse(V(G)$name=='94019245', 'blue', 'red')
#E(G)$color<-ifelse(E(G)$spendingItemAmount>1500000, "red", ifelse(E(G)$spendingItemAmount>750000, "orange", "green"))
#V(G)$size<-igraph::degree(G)/10
#V(G)[1]$size<-5
#par(mai=c(0,0,1,0))

# Generate colors base on media type:
V(G)$color <- ifelse(V(G)$name=='94019245', 'tomato', 'gold')

# Compute node degrees (#links) and use that to set node size:
V(G)$size <- igraph::degree(G)
V(G)[1]$size<-5 #Set it standard as a core vertex.

# The labels are currently node IDs.
# Setting them to NA will render no labels:
#V(G)$label <- NA
V(G)$label.cex <- 0.75
V(G)$label.color <- 'grey33'

# Set edge width based on weight:
E(G)$width <- E(G)$spendingItemAmount/1000000

# Set edge color based on weight:
colrs <- c("violetred", "springgreen3", "steelblue1", "sienna2")
E(G)$color<-ifelse(E(G)$spendingItemAmount > 1500000, colrs[1], 
	ifelse(E(G)$spendingItemAmount > 750000, colrs[2], 
	ifelse(E(G)$spendingItemAmount > 200000, colrs[3], colrs[4])))

#change arrow size and edge color:
E(G)$arrow.size <- .2
#E(G)$width <- .2

l <- layout.fruchterman.reingold(G)
plot(G, layout=l)
title(main="OTE Payments", col.main="tomato")
legend(x=-1.5, y=-1.1, c("Payment over 1.500.000€","Payment over 750.000€", 
	"Payment over 200.000€", "Rest of the payment amounts"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
#dev.off()
