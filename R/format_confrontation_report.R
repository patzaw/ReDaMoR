###############################################################################@
#' Format confrontation report for printing in console
#'
#' @param cr the confrontation report from [confront_data]
#' @param title a character with a single value corresponding to the report
#' title (e.g. database/model name)
#'
#' @example inst/examples/ex_confront.R
#'
#' @export
#'
format_confrontation_report <- function(
   cr,
   title="Model"
){

   ## Helpers ----
   successTag <- function(s){
      if(s) crayon::bgGreen(crayon::white('SUCCESS'))
      else crayon::bgRed(crayon::white('FAILURE'))
   }
   messageTag <- function(m){
      crayon::bgMagenta(crayon::white(m))
   }

   toRet <- c()

   ## Title ----
   toRet <- c(toRet, crayon::bold(crayon::underline(title)))

   ## Global success ----
   toRet <- c(toRet, successTag(cr$success))

   ## Global config ----
   toRet <- c(
      toRet,
      '',
      crayon::bold('Check configuration'),
      sprintf('   - Optional checks: %s', paste(cr$checks, collapse=", ")),
      sprintf('   - Maximum number of records: %s', cr$n_max),
      ''
   )

   ## Missing tables ----
   if(length(cr$missingTables)>0){
      toRet <- c(
         toRet,
         crayon::bold('Missing tables'),
         'The following tables are missing:',
         paste("   -", cr$missingTable),
         ''
      )
   }

   ## Supplementary tables ----
   if(length(cr$suppTables)>0){
      toRet <- c(
         toRet,
         crayon::bold('Not supported tables'),
         'The following tables are not supported by the model:',
         '',
         paste("   -", cr$suppTables),
         ''
      )
   }

   ## Table information ----
   ttoRet <- unlist(lapply(
      names(cr$constraints),
      function(tn){
         tcr <- cr$constraints[[tn]]
         toRet <- c()

         ## _+ Missing fields ----
         if(length(tcr$missingFields)>0){
            toRet <- c(
               toRet,
               crayon::underline('Missing fields'),
               'The following fields are missing:',
               paste("   -", tcr$missingFields),
               ''
            )
         }

         ## _+ Supplementary fields ----
         if(length(tcr$suppFields)>0){
            toRet <- c(
               toRet,
               crayon::underline('Not supported fields'),
               'The following fields are not supported by the model:',
               paste("   -", tcr$suppFields),
               ''
            )
         }

         ## _+ Field information ----
         ftoRet <- unlist(lapply(
            names(tcr$fields),
            function(fn){
               s <- tcr$fields[[fn]]$success
               m <- tcr$fields[[fn]]$message
               if(!s || (!is.null(m) && !is.na(m) && m!="")){
                  return(paste0(
                     '   - ', fn, ': ',
                     successTag(s), ' ',
                     messageTag(m)
                  ))
               }else{
                  return(NULL)
               }
            }
         ))
         if(length(ftoRet)>0){
            toRet <- c(
               toRet,
               crayon::underline('Field issues or warnings'),
               ftoRet,
               ''
            )
         }

         ## _+ Index information ----
         if(length(tcr$indexes)>0){
            itoRet <- unlist(lapply(
               1:length(tcr$indexes),
               function(i){
                  idx <- paste(cr$model[[tn]]$indexes[[i]]$fields, collapse="+")
                  if(cr$model[[tn]]$indexes[[i]]$unique){
                     idx <- paste(idx, '(unique)')
                  }
                  s <- tcr$indexes[[i]]$success
                  m <- tcr$indexes[[i]]$message
                  if(!s || (!is.null(m) && !is.na(m) && m!="")){
                     return(paste0(
                        '   - ', idx, ': ',
                        successTag(s), ' ',
                        messageTag(m)
                     ))
                  }else{
                     return(NULL)
                  }
               }
            ))
            if(length(itoRet)>0){
               toRet <- c(
                  toRet,
                  crayon::underline('Index issues or warnings'),
                  itoRet,
                  ''
               )
            }
         }

         ## _+ Foreign key information ----
         if(length(tcr$foreignKey)>0){
            fktoRet <- unlist(lapply(
               1:length(tcr$foreignKey),
               function(i){
                  fk <- paste(
                     cr$model[[tn]]$foreignKey[[i]]$key$from,
                     cr$model[[tn]]$foreignKey[[i]]$key$to,
                     sep="->"
                  ) %>% paste(collapse=" + ")
                  fk <- paste0(
                     cr$model[[tn]]$foreignKey[[i]]$refTable,
                     ' [', fk, ']'
                  )
                  s <- tcr$foreignKey[[i]]$success
                  if(is.null(s)) print(tcr)
                  m <- tcr$foreignKey[[i]]$message
                  if(!s || (!is.null(m) && !is.na(m) && m!="")){
                     return(paste0(
                        '   - ', fk, ': ',
                        successTag(s), ' ',
                        messageTag(m)
                     ))
                  }else{
                     return(NULL)
                  }
               }
            ))
            if(length(fktoRet)>0){
               toRet <- c(
                  toRet,
                  crayon::underline('Foreign keys issues or warnings'),
                  fktoRet,
                  ''
               )
            }
         }

         ## _+ Results if anything to show ----
         if(length(toRet)>0 || !tcr$success){
            toRet <- c(
               crayon::bold(tn),
               successTag(tcr$success),
               toRet,
               ''
            )
         }
      }
   ))

   if(length(ttoRet)>0){
      toRet <- c(
         toRet,
         ttoRet
      )
   }

   ## Concatenate the result ----
   return(paste(toRet, collapse="\n"))
}

###############################################################################@
#' Format confrontation report in markdown format
#'
#' @param cr the confrontation report from [confront_data]
#' @param title a character with a single value corresponding to the report
#' @param level rmarkdown level in document hierarchy (default:0 ==> highest).
#' It should be an integer between 0 and 4.
#' @param numbered a logical. If TRUE (default) the sections are part of
#' document numbering.
#' @param bgSuccess background color for SUCCESS
#' @param txSuccess text color for SUCCESS
#' @param bgFailure background color for FAILURE
#' @param txFailure text color for FAILURE
#' @param bgMessage background color for a warning message
#' @param txMessage text color for a warning message
#'
#' @example inst/examples/ex_confront.R
#'
#' @export
#'
format_confrontation_report_md <- function(
   cr,
   title="Model",
   level=0,
   numbered=TRUE,
   bgSuccess="green",
   txSuccess="black",
   bgFailure="red",
   txFailure="white",
   bgMessage="#FFBB33",
   txMessage="white"
){

   level <- round(level, digits=0)
   stopifnot(level >=0, level <=4)

   ## Helpers ----
   successTag <- function(s){
      sprintf(
         paste0(
            '<span ',
            'style="background-color:%s; color:%s; padding:2px;"',
            '>%s</span>'
         ),
         ifelse(s, bgSuccess, bgFailure),
         ifelse(s, txSuccess, txFailure),
         ifelse(s, 'SUCCESS', 'FAILURE')
      )
   }
   messageTag <- function(m){
      sprintf(
         '<span style="background-color:%s; color:%s; padding:2px;">%s</span>',
         bgMessage, txMessage, m
      )
   }

   toRet <- c()

   ## Title ----
   toRet <- c(toRet, '', sprintf('# %s', title[1]), '')

   ## Global success ----
   toRet <- c(toRet, successTag(cr$success))

   ## Global config ----
   toRet <- c(
      toRet,
      '',
      '## Check configuration',
      '',
      sprintf('- **Optional checks**: %s', paste(cr$checks, collapse=", ")),
      sprintf('- **Maximum number of records**: %s', cr$n_max),
      ''
   )

   ## Missing tables ----
   if(length(cr$missingTables)>0){
      toRet <- c(
         toRet,
         '',
         '## Missing tables',
         '',
         'The following tables are missing:',
         '',
         paste("-", cr$missingTable),
         ''
      )
   }

   ## Supplementary tables ----
   if(length(cr$suppTables)>0){
      toRet <- c(
         toRet,
         '',
         '## Not supported tables',
         '',
         'The following tables are not supported by the model:',
         '',
         paste("-", cr$suppTables),
         ''
      )
   }

   ## Table information ----
   ttoRet <- unlist(lapply(
      names(cr$constraints),
      function(tn){
         tcr <- cr$constraints[[tn]]
         toRet <- c()

         ## _+ Missing fields ----
         if(length(tcr$missingFields)>0){
            toRet <- c(
               toRet,
               '',
               '### Missing fields',
               '',
               'The following fields are missing:',
               '',
               paste(paste("-", tcr$missingFields)),
               ''
            )
         }

         ## _+ Supplementary fields ----
         if(length(tcr$suppFields)>0){
            toRet <- c(
               toRet,
               '',
               '### Not supported fields',
               '',
               'The following fields are not supported by the model:',
               '',
               paste(paste("-", tcr$suppFields)),
               ''
            )
         }

         ## _+ Field information ----
         ftoRet <- unlist(lapply(
            names(tcr$fields),
            function(fn){
               s <- tcr$fields[[fn]]$success
               m <- tcr$fields[[fn]]$message
               if(!s || (!is.null(m) && !is.na(m) && m!="")){
                  return(paste0(
                     '- ', fn, ': ',
                     successTag(s), ' ',
                     messageTag(m)
                  ))
               }else{
                  return(NULL)
               }
            }
         ))
         if(length(ftoRet)>0){
            toRet <- c(
               toRet,
               '',
               '### Field issues or warnings',
               '',
               ftoRet,
               ''
            )
         }

         ## _+ Index information ----
         if(length(tcr$indexes)>0){
            itoRet <- unlist(lapply(
               1:length(tcr$indexes),
               function(i){
                  idx <- paste(cr$model[[tn]]$indexes[[i]]$fields, collapse="+")
                  if(cr$model[[tn]]$indexes[[i]]$unique){
                     idx <- paste(idx, '(unique)')
                  }
                  s <- tcr$indexes[[i]]$success
                  m <- tcr$indexes[[i]]$message
                  if(!s || (!is.null(m) && !is.na(m) && m!="")){
                     return(paste0(
                        '- ', idx, ': ',
                        successTag(s), ' ',
                        messageTag(m)
                     ))
                  }else{
                     return(NULL)
                  }
               }
            ))
            if(length(itoRet)>0){
               toRet <- c(
                  toRet,
                  '',
                  '### Index issues or warnings',
                  '',
                  itoRet,
                  ''
               )
            }
         }

         ## _+ Foreign key information ----
         if(length(tcr$foreignKey)>0){
            fktoRet <- unlist(lapply(
               1:length(tcr$foreignKey),
               function(i){
                  fk <- paste(
                     cr$model[[tn]]$foreignKey[[i]]$key$from,
                     cr$model[[tn]]$foreignKey[[i]]$key$to,
                     sep="->"
                  ) %>% paste(collapse=" + ")
                  fk <- paste0(
                     cr$model[[tn]]$foreignKey[[i]]$refTable,
                     ' [', fk, ']'
                  )
                  s <- tcr$foreignKey[[i]]$success
                  if(is.null(s)) print(tcr)
                  m <- tcr$foreignKey[[i]]$message
                  if(!s || (!is.null(m) && !is.na(m) && m!="")){
                     return(paste0(
                        '- ', fk, ': ',
                        successTag(s), ' ',
                        messageTag(m)
                     ))
                  }else{
                     return(NULL)
                  }
               }
            ))
            if(length(fktoRet)>0){
               toRet <- c(
                  toRet,
                  '',
                  '### Foreign keys issues or warnings',
                  '',
                  fktoRet,
                  ''
               )
            }
         }

         ## _+ Results if anything to show ----
         if(length(toRet)>0 || !tcr$success){
            toRet <- c(
               '',
               sprintf('## %s', tn),
               '',
               successTag(tcr$success),
               '',
               toRet,
               ''
            )
         }
      }
   ))

   if(length(ttoRet)>0){
      toRet <- c(
         toRet,
         ttoRet
      )
   }

   ## Set document level ----
   toRet <- sub(
      "^[#]",
      paste0("#", paste(rep("#", level), collapse="")),
      toRet
   )
   ## Unnumber if requested ----
   if(!numbered){
      h <- grep("^[#]", toRet)
      toRet[h] <- paste(toRet[h], "{.unlisted .unnumbered}")
   }

   ## Concatenate the result ----
   toRet <- paste(toRet, collapse="\n")
   return(toRet)
}

###############################################################################@
#' View confrontation report in rstudio viewer
#'
#' @param cr the confrontation report from [confront_data]
#' @param ... additional params for
#' the [format_confrontation_report_md()] function
#'
#' @export
#'
view_confrontation_report <- function(cr, ...){
   tf <- tempfile(fileext=".html")
   on.exit(rm(tf))
   md <- format_confrontation_report_md(cr, ...)
   md <- markdown::renderMarkdown(text=md)
   writeLines(md, tf)
   rstudioapi::viewer(tf)
}
