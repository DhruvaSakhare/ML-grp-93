bmplot <- function(rowlabels, columnlabels, mat, main='Attributes selected', xlab='Iteration', ylab=''){
# rowlabels: Labels for the rows.
# columnlabels: Labels for the columns.
# mat: The binary matrix to be plotted.
# main, xlab, and ylab: These are passed on to the plotting
#  function, retaining their usual meaning in plots.
#
# Author: Laura Frølich, lff@imm.dtu.dk

# make room for horizontal y-labels
  par(mar = c(4, 7, 4, 2) + 0.1)
  I <- dim(mat)[1]
  M <- dim(mat)[2]

  image(1:(I+1), 1:(M+1), mat, col=gray((32:0)/32), xaxt="n", yaxt="n", xlab=xlab, ylab=ylab, main=main)

xseq <- 1:I
yseq <- 1:M
axis(1, at=xseq, labels=columnlabels)
axis(2, at=yseq+diff(yseq)[1]/2,labels=FALSE)

text(par("usr")[1] - 0.25, yseq+diff(yseq)[1]/2, srt = 0, adj = 1, labels = rowlabels, xpd = TRUE)

for(ixseq in xseq){
abline(v=ixseq)
}
for(iyseq in yseq){
abline(h=iyseq)
}
}
