# Regenerate the four saved convergence figures only when their data or style changes.
# Run manually from the presentation project directory:
# source("scripts/generate_tuesdaymeeting2p_plots.R")

library(ggplot2)

# The generator reads saved experiments; it does not rerun the PDE studies.
results_dir <- "data_files/tuesdaymeeting2p"
r_m <- readRDS(file.path(results_dir, "isolated_m_results_K16.rds"))
read_error_snapshot <- function(file) {
  env <- new.env(parent=emptyenv())
  load(file.path(results_dir, file), envir=env)
  value <- env$errors_projected
  list(errors=value, parameters=attr(value, "parameters"))
}
r_tau <- read_error_snapshot("errors_projected_tau_mat_K16.RData")
r_h <- read_error_snapshot("errors_projected_h_mat_K16.RData")
r_rough <- readRDS(file.path(results_dir, "rough_h_as_in_mat_results.rds"))
alphas <- r_m$parameters$alpha_vector
initial_modes <- c(5L, 6L, 8:21)
stopifnot(length(alphas)==9L,
  isTRUE(all.equal(alphas, seq(0.9,1.7,0.1))),
  identical(r_m$parameters$initial_modes, initial_modes),
  identical(r_tau$parameters$initial_modes, initial_modes),
  identical(r_h$parameters$initial_modes, initial_modes),
  isTRUE(all.equal(r_tau$parameters$alpha, alphas)),
  isTRUE(all.equal(r_h$parameters$alpha, alphas)),
  isTRUE(all.equal(r_rough$parameters$alpha, alphas)),
  identical(dim(r_h$errors), c(5L,9L)),
  identical(dim(r_tau$errors), c(5L,9L)),
  identical(dim(r_rough$errors), c(5L,9L)),
  all(is.finite(r_m$results$error)), all(r_m$results$error>0),
  all(is.finite(r_tau$errors)), all(r_tau$errors>0),
  all(is.finite(r_h$errors)), all(r_h$errors>0),
  all(is.finite(r_rough$errors)), all(r_rough$errors>0))

tau_rates <- apply(r_tau$errors,2,function(e)
  unname(coef(lm(log10(e)~log10(r_tau$parameters$axis)))[2]))
h_rates <- apply(r_h$errors,2,function(e)
  unname(coef(lm(log10(e)~log10(r_h$parameters$axis)))[2]))
alpha_colors <- setNames(scales::hue_pal()(length(alphas)),as.character(alphas))
# Match the minimal Palatino/LaTeX styling of NAP/about.Rmd.
plot_theme <- theme_minimal(base_size=13, base_family="serif") + theme(
  legend.position="bottom", legend.title=element_text(face="plain"),
  legend.key.width=grid::unit(1.1,"cm"),
  legend.spacing.x=grid::unit(0.12,"cm"),
  legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0),
  plot.margin=margin(6,10,4,6),
  plot.background=element_rect(fill="white",colour=NA))
alpha_scale <- function() scale_color_manual(values=alpha_colors,drop=FALSE,
  limits=as.character(alphas),
  labels=paste0("$",formatC(alphas,format="f",digits=1),"$"),
  name="$\\alpha$",guide=guide_legend(nrow=1,byrow=TRUE))

# Compile each plot separately with the same LaTeX font packages as about.Rmd.
# All experiment data still come from the saved snapshots above.
render_convergence_plot <- function(plot, name, width=10, height=3.6) {
  figure_dir <- file.path(results_dir,"convergence-plots")
  dir.create(figure_dir,recursive=TRUE,showWarnings=FALSE)
  build_dir <- tempfile(paste0(name,"-"))
  dir.create(build_dir)
  on.exit(unlink(build_dir,recursive=TRUE),add=TRUE)
  tex_file <- file.path(build_dir,paste0(name,".tex"))
  old_options <- options(tikzMetricsDictionary=
    file.path(tempdir(),"tuesdaymeeting2p-tikz-metrics"))
  on.exit(options(old_options),add=TRUE)
  tikzDevice::tikz(tex_file,standAlone=TRUE,
    width=width,height=height,
    documentDeclaration="\\documentclass[tikz,border=0pt]{standalone}",
    packages=c("\\usepackage{tikz}","\\usetikzlibrary{calc}",
      "\\usepackage[T1]{fontenc}","\\usepackage[sc]{mathpazo}",
      "\\usepackage{amsmath}"))
  tryCatch(print(plot),finally=grDevices::dev.off())
  build_log <- file.path(build_dir,"build.log")
  status <- system2("pdflatex",c("-interaction=nonstopmode","-halt-on-error",
    paste0("-output-directory=",shQuote(build_dir)),shQuote(tex_file)),
    stdout=build_log,stderr=build_log)
  if(status!=0L)
    stop("LaTeX plot compilation failed:\n",paste(readLines(build_log),collapse="\n"))
  figure_stem <- file.path(figure_dir,name)
  pdf_file <- paste0(figure_stem,".pdf")
  stopifnot(file.copy(file.path(build_dir,paste0(name,".pdf")),
    pdf_file,overwrite=TRUE))
  status <- system2("pdftoppm",c("-png","-singlefile","-r","240",
    shQuote(pdf_file),shQuote(figure_stem)),stdout=FALSE,stderr=FALSE)
  if(status!=0L) stop("LaTeX plot PNG conversion failed: ",name)
  invisible(paste0(figure_stem,".png"))
}
convergence_plot <- function(errors,axis,powers,x_label,y_label,
                             model_factor=rep(1,length(axis))) {
  d <- expand.grid(row=seq_along(axis),column=seq_along(alphas))
  d$x <- axis[d$row]
  d$error <- as.vector(errors)
  d$alpha <- factor(alphas[d$column],levels=alphas)
  guide <- d
  for(j in seq_along(alphas)) {
    ids <- d$column==j
    model <- axis^rep(powers,length.out=length(alphas))[j]*model_factor
    guide$error[ids] <- mean(errors[,j]/model)*model
  }
  ggplot(d,aes(x,error,color=alpha,group=alpha)) +
    geom_line(data=guide,linetype="dashed",linewidth=0.5) +
    geom_line(linewidth=0.5)+geom_point(size=2)+
    scale_x_log10(breaks=sort(unique(axis)),
      labels=scales::label_number(accuracy=NULL))+
    scale_y_log10(labels=scales::label_scientific())+
    alpha_scale()+labs(x=x_label,y=y_label)+
    plot_theme
}

# isolated-m
d <- r_m$results
d$alpha_label <- factor(d$alpha,levels=alphas)
guide <- d
for(a in alphas) {
  ids <- abs(d$alpha-a)<1e-8
  model <- exp(-2*pi*sqrt(a/2)*sqrt(d$m[ids]))
  guide$error[ids] <- mean(d$error[ids]/model)*model
}
p <- ggplot(d,aes(sqrt(m),error,color=alpha_label,group=alpha_label))+
  geom_line(data=guide,linetype="dashed",linewidth=0.5)+
  geom_line(linewidth=0.5)+geom_point(aes(shape=resolved),size=2)+
  scale_shape_manual(values=c(`FALSE`=1,`TRUE`=16),guide="none")+
  scale_x_continuous(breaks=sqrt(2:16),labels=2:16)+
  scale_y_continuous(trans=scales::log_trans(base=exp(1)),
    breaks=10^seq(-12,0,2),labels=scales::label_scientific())+
  alpha_scale()+labs(x="$m\\quad(\\sqrt{\\phantom{x}}\\mbox{ scale})$",
    y="$E_m\\quad(\\ln\\mbox{ scale})$")+
  plot_theme
render_convergence_plot(p,"isolated-m",height=4.1)

# temporal-convergence
p <- convergence_plot(r_tau$errors,r_tau$parameters$axis,1,
  "$\\tau\\quad(\\log_{10}\\mbox{ scale})$",
  "$E_{\\mathrm{sampled}}\\quad(\\log_{10}\\mbox{ scale})$")
render_convergence_plot(p,"temporal-convergence")

# smooth-spatial-convergence
p <- convergence_plot(r_h$errors,r_h$parameters$axis,2,
  "$h\\quad(\\log_{10}\\mbox{ scale})$",
  "$E_{\\mathrm{sampled}}\\quad(\\log_{10}\\mbox{ scale})$")+
  scale_x_log10(breaks=r_h$parameters$axis,
    labels=paste0("1/",round(1/r_h$parameters$axis)))
render_convergence_plot(p,"smooth-spatial-convergence")

# rough-corrected
p <- convergence_plot(r_rough$errors*r_rough$parameters$J,
  r_rough$parameters$h,alphas,
  "$h\\quad(\\log_{10}\\mbox{ scale})$",
  "$J E_{\\mathrm{sampled}}\\quad(\\log_{10}\\mbox{ scale})$")+
  scale_x_log10(breaks=sort(r_rough$parameters$h),
    labels=function(x)paste0("1/",round(1/x)))
render_convergence_plot(p,"rough-corrected")

# The presentation loads these precomputed slopes for the time and smooth-space studies.
saveRDS(list(temporal_rates=tau_rates,spatial_rates=h_rates),
  file.path(results_dir,"convergence-plots","plot-summary.rds"))
message("Saved the four convergence plots and their summary in ",
  file.path(results_dir,"convergence-plots"))
