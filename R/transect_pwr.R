#' Power analysis function for transect design
#'
#'
#' Taking a single set of transects from a HiDef digital aerial survey
#' will compute the statistical power for detecting population change.
#' @param CC An sf object. The opened cent count shapefile to process
#' @param Species A vector. The vector of species (using the two letter code)
#' that will be tested. E.G., c("S_CX","S_SP","S_RH","S_GN","S_GD","S_CA")
#' @param nseq A vector (optional). The number of transects to test. If NULL
#' then the code will test a halving, a doubling, and 1.5 times the spacing
#' of the original transects.
#' @param t.space Numeric. A value representing the transect spacing in the
#' cent count.
#' @param kseq Numeric sequence. Defaults to seq(0,1,by=0.1). The vector of
#' effect sizes to test (i.e., the % of original population size for which
#' statistical power is to be tested).
#' @param m Integer. The number of times transects are sampled. Default = 10
#' @param nr Integer. The number of replicate distribution samples to run.
#' @param plot Boolean. Whether or not to plot the output in the console. A plot
#' will be returned in the output list.
#' @param by.density Boolean. If TRUE, will use density rather than count
#' @param alternative indicates the alternative hypothesis and must be one of
#' "two.sided" (default), "less", or "greater". You can specify just the initial
#' letter of the value, but the argument name must be given in full. See
#' ‘Details’ for the meanings of the possible values.
#' @return A list. Contains plotobj (the plot as a ggplot), powervals (the data
#' frame with power values), and samplesize (the sample sizes as a data frame)
#'
#' @export
#' @import tidyverse
#' @import sf
#' @import broom
#' @import foreach
#' @import stats
#' @import extrafont
#' @examples
#' CentCount <- "D:/Power_Analysis/Data/Zone113_M02_S01_21_Output/Zone113_M02_S01_21_Output-CentCount.shp"
#' ## Read the CentCount and convert to a data frame
#' CC <- sf::st_read(CentCount)
#'
#' ## Number of transect samples
#' m <- 2
#' ## Number of replicates
#' nr <- 100
#' ## Create the sequences of the original population (effect size)
#' kseq <- seq(0,1,by=0.1)
#' ## Create the number of transects to test
#' nseq <- c(21,43,64,86)
#' labs <- c("5km","2.5km","1.68km","1.25km")
#' names(labs) <- nseq
#' Species <- c("S_CX","S_SP","S_RH","S_GN","S_GD","S_CA")
#' poweranalysis <- transect.pwr(CC=CC,Species = Species,nseq = nseq,t.space = 2.5,m = m,nr = nr, plot = TRUE, alternative = "less")



transect.pwr <- function(CC,Species,nseq=NULL,t.space,kseq=seq(0,1,by=0.1),
                         m=10,nr=1000,plot=TRUE,by.density=FALSE, alternative = "two.sided"){
  ## Load fonts for plot
  extrafont::loadfonts(quiet = TRUE)
  ### Validation checks

  ### Check Cent Count
  if(class(CC)[1]!="sf"){
    stop("D'OH! Cent Count is not an sf object!")
  }
  if(!identical(names(CC)[1:4],c("seg_len","seg_area","transect","reel"))){
    stop("Check that CC is actually a Cent Count shape file! Names do not match")
  }

  ### Check Species
  if(!all(Species %in% names(CC))){
    stop("Species listed are not in the Cent Count")
  }

  ### Check nseq
  if(!all(nseq-floor(nseq)==0)){
    stop("not all values of nseq are integers..")
  }

  ### Check t.space
  if(!is.numeric(t.space)){
    stop("t.space is not numeric - input the spacing between transects")
  }

  ## Convert to a CC
  CCdf <- as.data.frame(CC)

  ## If nseq is null, then calculate a halving, 1.5 times, and a doubling
  base <- length(unique(CC$transect))
  if(is.null(nseq)){
    half <- ceiling(base/2)
    onepfive <- ceiling(base*1.5)
    doub <- ceiling(base*2)
    nseq <- c(half,base,onepfive,doub)

    tspace <- c(round(t.space*2,2),t.space,round(t.space/1.5,2),round(t.space/2,2))
  }

  ## Calculate the transect spacing and apply to the data frame for naming
  #tspace <- round(length(unique(CC$transect))/nseq,2)*t.space
  #labs <- paste0(tspace,"km")
  #names(labs) <- nseq
  labs <- names(nseq)
  names(labs) <- nseq
  ## Get species labels from SppLookup
  Species.Labels <- left_join(data.frame(BTO_CODE=substr(Species,3,4)), SppLookup,by="BTO_CODE")$Species


  samplesize <- matrix(nrow=length(Species.Labels),ncol=3)

  spec_count <- 1  ## Counter for building the samplesize matrix
  finalall <- foreach(Spec=Species,.combine='rbind',.errorhandling='remove') %do% {
    cat(Spec,"\n")
    samplesize[spec_count,1] <- Species.Labels[spec_count]
    cat("##################################################################\n")

    ## Select the species of interest and get the coordinates
    CCsb <- CCdf %>% dplyr::select(transect,seg_area,all_of(Spec))
    names(CCsb)[3] <- "Species"
    CCsb <- cbind(CCsb,sf::st_coordinates(CC))
    ## Check number of animals
    cat(sum(CCsb$Species),"\n")
    samplesize[spec_count,2] <- sum(CCsb$Species)

    ### If doing by density, then change so Species column is a density
    if(by.density==TRUE){ CCsb$Species <- CCsb$Species/CCsb$seg_area}


    ## Check number of transects with observations
    summ <- CCsb %>% dplyr::group_by(transect) %>% summarise(total=sum(Species))
    n.transects <- length(which(summ$total > 0))
    cat(n.transects,"transects with observations \n")
    samplesize[spec_count,3] <- n.transects
    spec_count <- spec_count + 1
    ### Top level loops through transects (nseq)
    final <- foreach(n=nseq,.combine='rbind',.errorhandling = "remove") %do% {
      cat(n,"\n")
      cat("###################################\n")
      ### Second level loops through effect size (kseq)
      powervals <- foreach(k=kseq,.combine='rbind',.errorhandling = "remove")%do%{
        cat(k,"\n")
        cat("------------------\n")
        ### Randomly sample the transect IDs 10 times
        allpvals <- foreach(b=1:m,.combine='c',.errorhandling = "remove") %do% {
          cat(b,"\n")
          trans.sample <- sample(unique(CCsb$transect),size = length(unique(CCsb$transect)),replace = TRUE)
          ## Extract the sum of the counts for each transect ID to give distribution
          ## of counts using the transect ID as the blocking structure
          Eout <- foreach(i=trans.sample,.combine='c') %do%{
            Extract <- sum(CCsb[CCsb$transect == i,"Species"])
            return(Extract)
          }
          ## The mean of these counts is the value of mu for rbinom
          mu <- mean(Eout)
          ## Run a quasipoisson glm with a random variable to get the
          ## dispersion parameter (Theta)
          CCmodnb <- glm(Eout~1,family=quasipoisson)
          ## Calculate Theta
          Theta <- sum(residuals(CCmodnb,type ="pearson")^2)/CCmodnb$df.residual

          ## Create 1000 random samples where:
          pval <- foreach(j=1:nr,.combine='c',.errorhandling = "remove")%do%{
            ### We create a random distribution using the mean and Theta
            ### that emulates the original data in a quasipoisson distribution
            y <- rnbinom(n = n, mu = mu, size = mu/(Theta - 1))
            ### This doesn't work if the effect size is 0, so make it uber small
            if(k == 0){
              k <- 0.00001
            }
            ### Then create a random distribution using the mean x effect size
            ### to simulate a decrease in the population size within a
            ### quasipoisson
            z <- rnbinom(n = n, mu = mu*(1+k), size = (mu*(1+k))/(Theta - 1))
            ### perform a one-tailed ks test
            m1 <- ks.test(y,z,alternative = alternative)
            m1$p.value
          }
          pval
        }
        ### Power is the proportion of significant differences
        ### across the simulations.
        powr <- length(which(allpvals < 0.05))/length(allpvals)
        effectsiz <- k
        df <- data.frame(power=powr,effectsz=round(k,2))

      }

      powervals$ntrans <- n

      return(powervals)
    }
    final$ntrans <- as.factor(final$ntrans)
    final$species <- Spec
    return(final)

  }

  ## Create a plot
  G <- ggplot(finalall) +
    geom_line(aes(x=effectsz*100,y=power,group=species,color=species),size=1.5)+
    geom_hline(aes(yintercept=0.8),linetype='dashed',size=1)+
    scale_color_brewer(palette = "Dark2",name="",
                       labels=Species.Labels)+
    xlab("% change") +
    ylab("Power")+
    facet_wrap(~ntrans,labeller = as_labeller(labs))+
    HiDef::Theme_HiDEF(axis_title_size = 16)+
    theme(legend.text = element_text(size=14))

  if(plot==TRUE){
    G
  }
  samplesize <- data.frame(samplesize)
  names(samplesize) <- c("Species","N Observations", "N transects with Obs")

  outlist <- list(plotobj=G,powervals=finalall,samplesize=samplesize)

  return(outlist)
}

