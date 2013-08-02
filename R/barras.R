barras <-
function(x){

    colores<-c(
        "#772953",
        "#AEA79F",
        "#333333",
        "#2A75A9",
        "#274257",
        "#d44616")
    
    labels<-rownames(x)
    
    barplot(x,
        col=colores,
        names.arg=row.names(x),
        cex.names=0.7,
        family="Times")
        #axis(1, labels = FALSE)
        axis(2,family="Times")

}

#colores<-c(
#"#7EB5D6",
#"#2A75A9",
#"#274257",
#"#DFC184",
#"#8F6048",
#"#644436")