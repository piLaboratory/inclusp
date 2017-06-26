## Projeta proporcao de PPI no IB
## Dado a proporcao no periodo e uma fracao de PPI em uma fracao de SISU
#' inicio ano inicial
#' fim ano final
#' sisu proporcao de vagas sisu
#' ppi proporcao de vagas ppi dentro do sisu
#' file arquivo com n de matriculados ppi e nao ppi em cada ano, conforme arquivo corIB2
#' N de vagas por ano
projIB <- function(inicio, fim, sisu, ppi, file=corIB2, vagas=120){
    hoje <- subset(file, fase=="matriculado"&ano>=inicio&ano<=fim)
    hoje2 <- subset(file, fase=="matriculado"&ano>inicio&ano<=fim)
    N.ppi <- with(hoje, sum(ppi)) # N ppi no periodo
    N.no.ppi <- with(hoje, sum(no.ppi)) # N nao ppi no periodo
    N.ppi2 <- with(hoje2, sum(ppi)) # N ppi no periodo menos o primeiro ano
    N.no.ppi2 <- with(hoje2, sum(no.ppi)) # N nao ppi no periodo menos o primeiro ano
    p.ppi <- N.ppi/(N.ppi+N.no.ppi) # Proporcao ataul de ppi entre matriculados
    sisu.ppi <- round(vagas*sisu*ppi,0) # N de vagas ppi no sisu
    proj.ppi <- round((vagas - sisu.ppi)*ppi) + sisu.ppi ## projecao do N de ppis no ano seguinte
    data.frame(
        N.ppi.atual=N.ppi,
        N.nao.ppi.atual=N.no.ppi,
        p.ppi.atual=p.ppi,
        p.ppi.seguinte = (N.ppi2 + proj.ppi) / (vagas + N.ppi2 + N.no.ppi2),
        p.ppi.final = ((vagas-sisu.ppi)*p.ppi + sisu.ppi) / vagas
                       )
}

## Projecao usando 2012-2015, 30% de vags SISU e metade delas para ppi
projIB(2011, 2015, sisu=0.3, ppi=0.5, vagas=120)
