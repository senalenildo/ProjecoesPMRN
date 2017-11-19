#Carregando as bases
base=read.csv("C:\\TCC\\dados.csv",sep=";",header=TRUE,dec = ',')
subsidios=read.csv("C:\\TCC\\Subsidios.csv",header=FALSE)
TMD=read.csv("C:\\TCC\\TMD.csv",sep=",",header=TRUE)
TabuaMort=read.csv("C:\\TCC\\TabuaVida.csv",sep=",",header=TRUE)

#Definindo valor da alÃ­quota
aliquota = 0.33

#Criando tabela com valores dos salÃ¡rios acima do teto do inss
tetoinss = 5531.31
subsidiosacimadoteto = subsidios-tetoinss
subsidiosacimadoteto[subsidiosacimadoteto<0] = 0

#Obtendo Ã­ndices das pessoas ativas e os Ã­ndices das pessoas vivas, da base
IndicesAtivos = which(base$Atividade==1)
IndicesVivos = which(base$Atividade<=3)

#A variÃ¡vel nÃ­vel teÃ³rico mostra o nÃ­vel do PM que ele deveria estar, desconsiderando os processos administrativos
base$NIVEL.TEORICO = NA

#NÃ­vel dos inativos serÃ¡ o mesmo nÃ­vel de quando ele saiu da ativa
base$NIVEL.TEORICO[-IndicesAtivos] = base$NIVELINATIVOS[-IndicesAtivos]

#Idade de aposentadoria serÃ¡ o menor valor entre a idade de entrada mais 30 e a idade de 56 anos.
#PorÃ©m, quem tem 56 anos ou mais e ainda estÃ¡ ativo, aposentarÃ¡ no ano seguinte
base$IDADE.APOSENTADORIA.COMP = base$IDADE.APOSENTADORIA.TEMPO = base$IDADE.APOSENTADORIA = NA
base$IDADE.APOSENTADORIA.COMP[IndicesAtivos] = 56
base$IDADE.APOSENTADORIA.TEMPO[IndicesAtivos] = (base$IDADE[IndicesAtivos]-base$TEMPO.ATIVO[IndicesAtivos])+30
base$IDADE.APOSENTADORIA[IndicesAtivos] = apply(base[IndicesAtivos,10:11],1,min)
#Quem tem 56 anos ou mais e ainda estÃ¡ ativo, aposentarÃ¡ no ano seguinte
base$IDADE.APOSENTADORIA[IndicesAtivos][base$IDADE[IndicesAtivos]>=56] = base$IDADE[IndicesAtivos][base$IDADE[IndicesAtivos]>=56] + 1  
#Quem tem 30 anos de contribuiÃ§Ã£o ou mais e ainda estÃ¡ ativo, aposentarÃ¡ no ano seguinte
base$IDADE.APOSENTADORIA[IndicesAtivos][base$TEMPO.ATIVO[IndicesAtivos]>=30] = base$IDADE[IndicesAtivos][base$TEMPO.ATIVO[IndicesAtivos]>=30] + 1  


#Calculando nivel que seria, teoricamente, de acordo com o tempo ativo
base$NIVEL.TEORICO[IndicesAtivos] = (base$TEMPO.ATIVO[IndicesAtivos]%/%3+1)
base$NIVEL.TEORICO[IndicesAtivos][base$NIVEL.TEORICO[IndicesAtivos]>10] = 10

#Vetores para auxiliar no cÃ¡lculo da mudanÃ§a de graduaÃ§Ã£o
TemposGrad = c(0,10,16,20,24,28)
AnosGrad = c(rep(1,9),rep(2,6),rep(3,4),rep(4,4),rep(5,4),rep(6,50))

#AdaptaÃ§Ã£o para que quem estiver em uma graduaÃ§Ã£o nÃ£o compatÃ­vel com o tempo de atividade
#(de acordo com a lei da promoÃ§Ã£o dos praÃ§as), sejam promovidos no ano seguinte
base$TEMPO.GRAD.ATUAL[IndicesAtivos][base$GRAD[IndicesAtivos]==1 & base$TEMPO.GRAD.ATUAL[IndicesAtivos]>=10] = 9
base$TEMPO.GRAD.ATUAL[IndicesAtivos][base$GRAD[IndicesAtivos]==2 & base$TEMPO.GRAD.ATUAL[IndicesAtivos]>=6] = 5
base$TEMPO.GRAD.ATUAL[IndicesAtivos][base$GRAD[IndicesAtivos]==3 & base$TEMPO.GRAD.ATUAL[IndicesAtivos]>=4] = 3
base$TEMPO.GRAD.ATUAL[IndicesAtivos][base$GRAD[IndicesAtivos]==4 & base$TEMPO.GRAD.ATUAL[IndicesAtivos]>=4] = 3
base$TEMPO.GRAD.ATUAL[IndicesAtivos][base$GRAD[IndicesAtivos]==5 & base$TEMPO.GRAD.ATUAL[IndicesAtivos]>=4] = 3

#Ajuste para definir que o tempo na graduaÃ§Ã£o atual de um pm, seja no mÃ¡ximo 1 ano a menos para ele ser promovido para a prÃ³xima graduaÃ§Ã£o
base$TEMPO.ATIVO.TEORICO = base$STATUS = NA
base$TEMPO.ATIVO.TEORICO[IndicesAtivos] = TemposGrad[base$GRAD[IndicesAtivos]]+base$TEMPO.GRAD.ATUAL[IndicesAtivos]
base$STATUS[IndicesAtivos] = AnosGrad[base$TEMPO.ATIVO.TEORICO[IndicesAtivos]]
base$STATUS[-IndicesAtivos] = base$GRAD[-IndicesAtivos]

#Criando listas que armazenarÃ£o resultados
lista.contagem.ativos.total = list()
lista.contagem.inativos.total = list()
lista.contribuicao = list()
lista.contribuicao.ativos = list()
lista.contribuicao.inativos = list()
lista.beneficios = list()
lista.estados = list()

elevaAoQuadrado = function(x) {
  return(x*x)
}

#Fazendo uma cÃ³pia da base
base1 = base

#Definindo quantas repetiÃ§Ãµes serÃ£o feitas da simulaÃ§Ã£o
rodadas = 1

for (r in 1:rodadas) {
  #criando matriz que armazenarÃ¡ a contagem do efetivo ano a ano
  contagem.ativos = matrix(0,6,10)
  lista.contagem.ativos = list()
  contagem.inativos = matrix(0,7,10)
  lista.contagem.inativos = list()
  
  #VarÃ­aveis que armazenarÃ£o alguns resultados
  SalarioContribuicao = Beneficios = ContribuicaoAtivos = ContribuicaoInativos = Contribuicao = 0
  contagem.ativos.total = matrix(0,7,0)
  contagem.inativos.total = matrix(0,8,0)
  estados = matrix(0,3,0)
  
  #Contador para contar quantos anos foram passados na simulaÃ§Ã£o
  i=0
  
  #Resetando para a base inicial, apÃ³s cada iteraÃ§Ã£o do laÃ§o while
  base = base1

  #Resetando tambÃ©m os Ã­ndices dos ativos e inativos
  IndicesAtivos = which(base$Atividade==1)
  IndicesVivos = which(base$Atividade<=3)
  
  #Mostrando na tela qual repetiÃ§Ã£o a microssimulaÃ§Ã£o estÃ¡ processando
  print(paste("Rodada",r))
  
  while(TRUE) {
    
    #CondiÃ§Ã£o de parada da projeÃ§Ã£o, quando todas os PMs estiverem mortos
    if(sum(base$Atividade==4)==nrow(base)) {
      break
    }
    i=i+1
    
    #CONTANDO ATIVOS
    #criando matriz que armazenarÃ¡ a contagem dos ativos ano a ano
    contagem.ativos = rbind(
      c(sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==1),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==2),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==3),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==4),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==5),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==6),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==7),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==8),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==9),sum(base$Atividade==1&base$STATUS==1&base$NIVEL.TEORICO==10)),
      c(sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==1),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==2),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==3),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==4),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==5),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==6),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==7),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==8),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==9),sum(base$Atividade==1&base$STATUS==2&base$NIVEL.TEORICO==10)),
      c(sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==1),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==2),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==3),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==4),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==5),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==6),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==7),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==8),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==9),sum(base$Atividade==1&base$STATUS==3&base$NIVEL.TEORICO==10)),
      c(sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==1),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==2),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==3),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==4),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==5),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==6),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==7),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==8),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==9),sum(base$Atividade==1&base$STATUS==4&base$NIVEL.TEORICO==10)),
      c(sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==1),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==2),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==3),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==4),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==5),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==6),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==7),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==8),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==9),sum(base$Atividade==1&base$STATUS==5&base$NIVEL.TEORICO==10)),
      c(sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==1),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==2),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==3),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==4),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==5),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==6),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==7),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==8),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==9),sum(base$Atividade==1&base$STATUS==6&base$NIVEL.TEORICO==10))
    )
    
    contagem.ativos = cbind(contagem.ativos,rowSums(contagem.ativos*subsidios[1:6,]))
    rownames(contagem.ativos) = c("Soldado","Cabo","3Âº Sargento","2Âº Sargento","1Âº Sargento","Subtenente")
    colnames(contagem.ativos) = c("I","II","III","IV","V","VI","VII","VIII","IX","X","RemuneraÃ§Ãµes")
    
    SalarioContribuicao[i] = sum(contagem.ativos[,11])
    contagem.ativos.total = cbind(contagem.ativos.total,c(rowSums(contagem.ativos[,1:10]),SalarioContribuicao[i]))
    lista.contagem.ativos[[i]] = contagem.ativos
    
    #CONTANDO INATIVOS
    #criando matriz que armazenarÃ¡ a contagem dos ativos ano a ano
    contagem.inativos = rbind(
      c(sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==1),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==2),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==3),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==4),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==5),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==6),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==7),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==8),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==9),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==1&base$NIVEL.TEORICO==10)),
      c(sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==1),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==2),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==3),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==4),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==5),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==6),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==7),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==8),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==9),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==2&base$NIVEL.TEORICO==10)),
      c(sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==1),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==2),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==3),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==4),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==5),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==6),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==7),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==8),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==9),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==3&base$NIVEL.TEORICO==10)),
      c(sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==1),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==2),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==3),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==4),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==5),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==6),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==7),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==8),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==9),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==4&base$NIVEL.TEORICO==10)),
      c(sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==1),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==2),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==3),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==4),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==5),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==6),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==7),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==8),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==9),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==5&base$NIVEL.TEORICO==10)),
      c(sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==1),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==2),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==3),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==4),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==5),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==6),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==7),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==8),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==9),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==6&base$NIVEL.TEORICO==10)),
      c(sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==1),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==2),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==3),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==4),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==5),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==6),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==7),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==8),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==9),sum((base$Atividade==2|base$Atividade==3)&base$STATUS==7&base$NIVEL.TEORICO==10))
    )
    
    contagem.inativos = cbind(contagem.inativos,rowSums(contagem.inativos*subsidios))
    rownames(contagem.inativos) = c("Soldado","Cabo","3Âº Sargento","2Âº Sargento","1Âº Sargento","Subtenente","3Âº Tenente")
    colnames(contagem.inativos) = c("I","II","III","IV","V","VI","VII","VIII","IX","X","Beneficio")
    
    Beneficios[i] = sum(contagem.inativos[,11])
    contagem.inativos.total = cbind(contagem.inativos.total,c(rowSums(contagem.inativos[,1:10]),Beneficios[i]))
    lista.contagem.inativos[[i]] = contagem.inativos
    
    ContribuicaoAtivos[i] = SalarioContribuicao[i]*aliquota
    ContribuicaoInativos[i] = sum(rowSums(contagem.inativos*subsidiosacimadoteto))*aliquota
    Contribuicao[i] = ContribuicaoAtivos[i] + ContribuicaoInativos[i]
    
    #Contando nÃºmero de ativos, inativos e mortos
    estados = cbind(estados,c(sum(base$Atividade==1),sum(base$Atividade==2|base$Atividade==3),sum(base$Atividade==4)))
    
    #Indo para o ano seguinte
    base$IDADE[IndicesVivos] = base$IDADE[IndicesVivos]+1
    base$TEMPO.ATIVO[IndicesAtivos] = base$TEMPO.ATIVO[IndicesAtivos]+1
    
    #Verificando quem mudou de graduaÃ§Ã£o
    base$TEMPO.ATIVO.TEORICO[IndicesAtivos] = base$TEMPO.ATIVO.TEORICO[IndicesAtivos]+1
    base$STATUS[IndicesAtivos] = AnosGrad[base$TEMPO.ATIVO.TEORICO[IndicesAtivos]]
    
    #Verificando quem mudou de nÃ­vel
    base$NIVEL.TEORICO[IndicesAtivos] = (base$TEMPO.ATIVO[IndicesAtivos]%/%3+1)
    base$NIVEL.TEORICO[IndicesAtivos][base$NIVEL.TEORICO[IndicesAtivos]>10] = 10
    
    #Definindo aleatoriamente quem invalidou e morreu
    base$Atividade[IndicesAtivos][base$IDADE[IndicesAtivos]>=base$IDADE.APOSENTADORIA[IndicesAtivos]] = 2
    SorteioInvalidos = rbinom(nrow(base[IndicesAtivos,]),1,TMD$tmdinv[base$IDADE[IndicesAtivos]])
    base$Atividade[IndicesAtivos][which(SorteioInvalidos==1)] = 3
    SorteioMortos =   rbinom(nrow(base[IndicesAtivos,]),1,TMD$tmdmorte[base$IDADE[IndicesAtivos]])
    base$Atividade[IndicesAtivos][which(SorteioMortos==1)] = 4
    IndicesInativos = which(base$Atividade==2|base$Atividade==3)
    SorteioMortos =   rbinom(nrow(base[IndicesInativos,]),1,TabuaMort$qx[base$IDADE[IndicesInativos]])
    base$Atividade[IndicesInativos][which(SorteioMortos==1)] = 4
    
    #Obtendo novos Ã­ndices das pessoas ativas e das pessoas vivas
    IndicesAtivos = which(base$Atividade==1)
    IndicesVivos = which(base$Atividade<=3)
  }
  
  #Nomeando linhas e colunas
  rownames(contagem.ativos.total) = c("Soldado","Cabo","3Âº Sargento","2Âº Sargento","1Âº Sargento","Subtenente","RemuneraÃ§Ã£o dos Ativos")
  colnames(contagem.ativos.total) = as.character((1:i)+2016)
  lista.contagem.ativos.total[[r]] = contagem.ativos.total
  rownames(contagem.inativos.total) = c("Soldado","Cabo","3Âº Sargento","2Âº Sargento","1Âº Sargento","Subtenente","3Âº Tenente","BenefÃ­cios dos Inativos")
  colnames(contagem.inativos.total) = as.character((1:i)+2016)
  lista.contagem.inativos.total[[r]] = contagem.inativos.total

  #Adicionando nas listas os resultados da simulaÃ§Ã£o de cada repetiÃ§Ã£o
  lista.beneficios[[r]] = Beneficios
  lista.contribuicao[[r]] = Contribuicao
  lista.contribuicao.ativos[[r]] = ContribuicaoAtivos
  lista.contribuicao.inativos[[r]] = ContribuicaoInativos
  lista.estados[[r]] = estados
}

#Calculando mÃ©dias das contagens de ativos
DimMediaAtivos = unlist(lapply(lista.contagem.ativos.total,ncol))
lista.contagem.ativos.total = lapply(lista.contagem.ativos.total,function(x) x[,1:(2040-2017+1)])
MediaAtivos = Reduce('+', lista.contagem.ativos.total)/length(lista.contagem.ativos.total)

#Calculando mÃ©dias das contagens de inativos
dimmax = max(unlist(lapply(lista.contagem.inativos.total,ncol)))
padroniza = function(x) cbind(x,matrix(0,nrow(x),(dimmax-ncol(x))))
lista.contagem.inativos.total = lapply(lista.contagem.inativos.total,padroniza)
MediaInativos = Reduce('+', lista.contagem.inativos.total)/length(lista.contagem.inativos.total)
colnames(MediaInativos) = as.character(2017:(ncol(MediaInativos)+2016))

#Calculando mÃ©dias das contagens dos estados (ativos, inativos ou mortos)
dimmax = max(unlist(lapply(lista.estados,ncol)))
padroniza = function(x) cbind(x,matrix(c(0,0,nrow(base)),nrow(x),(dimmax+1-ncol(x))))
lista.estados = lapply(lista.estados,padroniza)
MediaEstados = Reduce('+', lista.estados)/length(lista.estados)
colnames(MediaEstados) = as.character(2017:(ncol(MediaEstados)+2016))
rownames(MediaEstados) = c("Ativos","Inativos","Mortos")

#Calculando mÃ©dia das contribuiÃ§Ãµes
tammax = max(unlist(lapply(lista.contribuicao,length)))
padroniza = function(x) c(x,rep(0,tammax-length(x)))
lista.contribuicao = lapply(lista.contribuicao,padroniza)
MediaContribuicao = Reduce('+', lista.contribuicao)/length(lista.contribuicao)

#Calculando mÃ©dia das contribuiÃ§Ãµes dos ativos
tammax = max(unlist(lapply(lista.contribuicao.ativos,length)))
lista.contribuicao.ativos = lapply(lista.contribuicao.ativos,padroniza)
MediaContribuicaoAtivos = Reduce('+', lista.contribuicao.ativos)/length(lista.contribuicao.ativos)

#Calculando mÃ©dia das contribuiÃ§Ãµes dos inativos
tammax = max(unlist(lapply(lista.contribuicao.inativos,length)))
lista.contribuicao.inativos = lapply(lista.contribuicao.inativos,padroniza)
MediaContribuicaoInativos = Reduce('+', lista.contribuicao.inativos)/length(lista.contribuicao.inativos)

#Calculando mÃ©dia dos benefÃ­cios
tammax = max(unlist(lapply(lista.beneficios,length)))
lista.beneficios = lapply(lista.beneficios,padroniza)
MediaBeneficios = Reduce('+', lista.beneficios)/length(lista.beneficios)

#Juntando resultados das contribuiÃ§Ãµes e benefÃ­cios
AtivoPassivo = rbind(MediaContribuicaoAtivos,MediaContribuicaoInativos,MediaContribuicao,MediaBeneficios)
colnames(AtivoPassivo) = as.character(2017:(ncol(AtivoPassivo)+2016))
rownames(AtivoPassivo) = c("ContribuiÃ§Ãµes dos Ativos","ContribuiÃ§Ãµes dos Inativos","ContribuiÃ§Ãµes Total","BenefÃ­cios")

#Salvando todos os resultados em arquivos .csv
write.csv(x = MediaAtivos,"ProjeÃ§Ã£o dos ativos por graduaÃ§Ã£o e remuneraÃ§Ãµes dos ativos.csv")
write.csv(x = MediaInativos,"ProjeÃ§Ã£o dos inativos por graduaÃ§Ã£o e benefÃ­cios dos ativos.csv")
write.csv(x = AtivoPassivo,"ProjeÃ§Ã£o das ContribuiÃ§Ãµes e BenefÃ­cios.csv")
write.csv(x = MediaEstados,"ProjeÃ§Ã£o dos Ativos, Inativos e Mortos.csv")
write.csv(x = table(base$IDADE),"Idade das mortes.csv",row.names = FALSE)
