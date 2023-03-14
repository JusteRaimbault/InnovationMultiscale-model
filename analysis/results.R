
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Models/InnovationMultiscale/InnovationMultiscale-model/openmole'))

library(dplyr,warn.conflicts = F)
library(readr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

params<-c("macroGravityDecay","macroInnovationDecay","mesoCrossOverProba","mesoMutationProba",
          "mesoInteractionProba","mesoToMacroInnovationThreshold","macroToMesoCrossoverMaxUpdate",
          "macroToMesoExchangeMaxUpdate","macroToMesoMutationMaxUpdate")

indics<-c("macroDiversity", "macroInnovation", "macroUtility", "mesoDiversity", "mesoFitness",
  "deltaDiversity","deltaUtility","gammaDiversity","gammaUtility","psiUtility","psiDiversity")
# todo: indics std

resprefix = '20230313_232838_EXPLORATION'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/InnovationMultiscale/',resprefix,'/');dir.create(resdir,recursive = T)

res <- read_csv(file=paste0('exploration/',resprefix,'.csv'))

# stochasticity
sres = res %>% group_by(id) %>% summarise(
  sdmacroDiversity=sd(macroDiversity),meanmacroDiversity=mean(macroDiversity),sharpemacroDiversity=abs(meanmacroDiversity/sdmacroDiversity),
  sdmacroInnovation=sd(macroInnovation),meanmacroInnovation=mean(macroInnovation),sharpemacroInnovation=abs(meanmacroInnovation/sdmacroInnovation),
  sdmacroUtility=sd(macroUtility),meanmacroUtility=mean(macroUtility),sharpemacroUtility=abs(meanmacroUtility/sdmacroUtility),
  sdmesoDiversity=sd(mesoDiversity),meanmesoDiversity=mean(mesoDiversity),sharpemesoDiversity=abs(meanmesoDiversity/sdmesoDiversity),
  sdmesoFitness=sd(mesoFitness),meanmesoFitness=mean(mesoFitness),sharpemesoFitness=abs(meanmesoFitness/sdmesoFitness)
)
summary(sres)

reldistance <- function(indic,sdindic){
  c(2*abs(matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = T) - matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = F))/(matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = T) + matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = F)))
}

summary(reldistance("meanmacroDiversity","sdmacroDiversity"))
summary(reldistance("meanmacroInnovation","sdmacroInnovation"))
summary(reldistance("meanmacroUtility","sdmacroUtility"))
summary(reldistance("meanmesoDiversity","sdmesoDiversity"))
summary(reldistance("meanmesoFitness","sdmesoFitness"))



# indicator plots
sres = res %>% group_by(mesoCrossOverProba,macroGravityDecay,macroInnovationDecay,mesoToMacroInnovationThreshold,macroToMesoExchangeMaxUpdate)%>%
  summarise(macroDiversity=mean(macroDiversity), macroInnovation=mean(macroInnovation),
  macroUtility=mean(macroUtility), mesoDiversity=mean(mesoDiversity), mesoFitness=mean(mesoFitness),
  deltaDiversity=mean(deltaDiversity),deltaUtility=mean(deltaUtility),gammaDiversity=mean(gammaDiversity),gammaUtility=mean(gammaUtility),psiUtility=mean(psiUtility),psiDiversity=mean(psiDiversity))

for(mesoCrossOverProba in unique(sres$mesoCrossOverProba)){
  for (indic in indics){
    ggsave(
      ggplot(sres[sres$mesoCrossOverProba==mesoCrossOverProba,],
             aes_string(x = "macroGravityDecay", y=indic, color = "macroInnovationDecay", group="macroInnovationDecay" ))+
        geom_line()+facet_grid(mesoToMacroInnovationThreshold~macroToMesoExchangeMaxUpdate,scales = 'free')+
        scale_colour_continuous(name=expression(d[I]))+xlab(expression(d[G]))+ylab(indic)+stdtheme
      ,filename = paste0(resdir,indic,'-macroGravityDecay_color-macroInnovationDecay_facet-mesoToMacroInnovationThreshold-macroToMesoExchangeMaxUpdate_mesoCrossOverProba',mesoCrossOverProba,'.png'),width=30,height=20,units='cm'
    )
  }
}


# optimisation

res <- read_csv(file=paste0('optimisation/20230314_084145_OPTIMISATION/population2851.csv'),col_names = T)
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/InnovationMultiscale/optimisation/20230314_084145_OPTIMISATION/');dir.create(resdir,recursive = T)


g=ggplot(res,aes(x=macroUtility,y=mesoDiversity,color=macroGravityDecay...3,size=mesoToMacroInnovationThreshold...8))
g+geom_point(alpha=0.6)+xlab("Macro utility")+ylab("Meso diversity")+
  scale_size_continuous(name=expression(theta))+scale_colour_continuous(name=expression(d[G]))+stdtheme
ggsave(filename = paste0(resdir,"paretoDiversity-Fitness_colordG_sizetheta.png"),width=23,height=20,units='cm')


# pse

res <- read_csv(file=paste0('pse/20230314_084046_PSE/population138.csv'),col_names = T)
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/InnovationMultiscale/pse/20230314_084046_PSE/');dir.create(resdir,recursive = T)


g=ggplot(res,aes(x=psiUtility,y=deltaUtility,color=gammaUtility,size=mesoToMacroInnovationThreshold...8))
g+geom_point(alpha=0.6)+xlab("Psi(utility)")+ylab("Delta(utility)")+
  scale_size_continuous(name=expression(theta))+scale_colour_continuous(name=expression(Gamma))+stdtheme
ggsave(filename = paste0(resdir,"pse-psi-delta-utility_colorGamma_sizetheta.png"),width=23,height=20,units='cm')






