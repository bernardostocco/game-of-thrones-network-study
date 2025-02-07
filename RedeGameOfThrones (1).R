#Instalando o pacote igraph

install.packages('igraph')

library(igraph)


#Lendo a lista de arestas disponibilizada no github e baixada no WD

lista_arestas <- read.csv(file = "got-edges.csv",
                          header = TRUE, sep = ",",
                          dec = ',')

is.data.frame(lista_arestas)

head(lista_arestas)


#Lendo a lista de v�rtices de arestas disponibilizada no github e baixada no WD

lista_vertices <- read.csv(file = "got-nodes.csv",
                           header = TRUE, sep = ",",
                           dec = ',')


#Verificando se o dataframe foi gerado

is.data.frame(lista_vertices)

head(lista_vertices)

#Grafo

grafo <- graph_from_data_frame(lista_arestas,
                               directed = FALSE,
                               vertices = lista_vertices)

tkplot(grafo,
       layout = layout.lgl,
       vertex.color="light green")

#Atributos dos v�rtices

vertex_attr(grafo)


#Atributos das arestas

edge_attr(grafo)


#Tirando o label do grafo

tkplot(grafo,
       layout = layout.lgl,
       vertex.color = "light green",
       vertex.label = NA)


#Resumo do grafo

summary(grafo)

degree(grafo)


#Calculando os graus do grafo

graus <- degree(grafo)

graus


#Calculando o maior grau do grafo

maiorGrau <- max (graus)

maiorGrau


#Calculando as intermedia��es dos v�rtices do grafo

intermediacaoVertices <- betweenness(grafo)

intermediacaoVertices


#Calculando a maior intermedia��o dos v�rtices do grafo

maiorIntermediacaoVertices <- max(intermediacaoVertices)

maiorIntermediacaoVertices


#Calculando a o di�metro do grafo

get_diameter(grafo)

diametro <- get_diameter(grafo)

diametro


#Calculando o histograma dos di�metros do grafo

hist(diametro)


#Calculando o di�metro

diametro <- diameter (grafo)

diametro

#Identificando os graus do grafo de forma crescente e decrescente

sort(degree(grafo), decreasing = TRUE) [1:10]

sort(degree(grafo), decreasing = FALSE) [1:30]


#Identificando em ordem decrescente, a maior proximidade e a maior
#intermedia��o dos v�rtices

sort(closeness(grafo), decreasing = TRUE) [1:10]

sort(betweenness(grafo), decreasing = TRUE) [1:10]


#Identificando o grau de um personagem em espec�fico

degree(grafo) ["Robert"]


#Pesquisando personagens que possuem grau maior que 30

degree(grafo)[degree(grafo) > 30]


#Pesquisando personagens que possuem grau menor que 20

degree(grafo)[degree(grafo) < 20]


#Pesquisando personagens que possuem o grau igual a 12

degree(grafo)[degree(grafo) == 12]


#Calculando o valor m�dio do grau os v�rtices que comp�em a rede

mean(degree(grafo))


#Conferindo os atributos dos v�rtices do grafo para plotar a rede,
#fazendo com que Tyrion, Jon e Sansa fiquem em evid�ncia, j� que s�o
#os personagens com os v�rtices mais centrais

vertex_attr(grafo)

#Plotando o grafo

plot(grafo,
     vertex.label = ifelse(V(grafo)$name %in% c("Tyrion", "Jon", "Sansa"),
                           V(grafo)$name,
                           NA),
     vertex.size = ifelse(V(grafo)$name %in% c("Tyrion", "Jon", "Sansa"),
                          30,
                          7),
     vertex.color = ifelse(V(grafo)$name %in% c("Tyrion", "Jon", "Sansa"),
                           "red",
                           NA))


#Plotando histogramas com novos comandos

#par

par(mfrow = c(1,2)
    , bty = "n")


#hist

hist(degree(grafo)
     ,col = "lightblue"
     ,main = "Aula 26-04-2022")


#stripchart

stripchart(degree(grafo)
           ,method = "stack"
           ,pch = 16
           ,cex = 1.2
           ,at = 0
           ,col = "lightblue")


#Calculando a centralidade de autovetor da rede

eigen_centrality(grafo)


#Calculando a centralidade de autovetor da rede de forma decrescente

sort(eigen_centrality(grafo)$vector, decreasin = TRUE) [1:10]


#Identificando os vizinhos com a fun��o neighbors (Jon possui 26 vizinhos,
# pois o grau dele � 26)

neighbors(grafo, "Jon")


#Para usar a fun��o neighborhood, � preciso encontrar personagens com menos
#vizinhos para que n�o fique t�o polu�do, j� que Jon possui 26 vizinhos

#Encontrando personagens que possuam apenas 1 vizinho

degree(grafo)[degree(grafo) == 1]

#O personagem escolhido foi a Amory, agora, utilizando a fun��o neighborhood,

neighborhood(grafo, order = 1, "Amory")


#Construindo subgrafos a partir do grafo original

grafo_sub <- subgraph.edges(grafo, 
                            E(grafo)[inc(c("Amory", "Oberyn"))])

plot(grafo_sub, vertex.color="lightblue", vertex.size=40)
                            
tkplot(grafo_sub,
       vertex.color="lightblue",
       vertex.size=40) 

grafo_sub <- subgraph.edges(grafo, 
                            E(grafo)[inc(c("Tyrion", "Jon"))])
                            
plot(grafo_sub, vertex.color="lightblue", vertex.size=40)

tkplot(grafo_sub,
       vertex.color="lightblue",
       vertex.size=40)


#Calculando a modularidade da comunidade 1

comunidade1 <- cluster_edge_betweenness(grafo)

modularity(comunidade1)    


#Plotando a comunidade separada em agrupamentos que tem
#como crit�rio principal a manipula��o de intermedia��o de arestas

plot(comunidade1
     ,grafo
     ,vertex.label =NA
     ,vertex.size = 10)


#Definindo os membros da comunidade

membros_da_comunidade1 <- membership(comunidade1)


#Exibindo uma tabela dos membros da comunidade. A primeira linha
#indica o grup, a linha de baixo n�mero de v�rtices de cada grupo.

table(membros_da_comunidade1)


#Fazendo o grafo com os r�tulos, sendo o n�mero do grupo a que
#o v�rtice pertence

plot(comunidade1
     ,grafo
     ,vertex.size = 20
     ,vertex.label = as.character(membros_da_comunidade1))


#Descobrindo os membros do grupo 8

membros_da_comunidade1[membros_da_comunidade1 ==8]


#Descobrindo os membros do grupo 6

membros_da_comunidade1[membros_da_comunidade1 == 6]


