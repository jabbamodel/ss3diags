#' @name pac.hke
#' @title 2020 Pacific Hake Stock Synthesis model
#' @description Information relevant to Pacific Hake (HKE) example, including
#' Stock Synthesis (V3.30.14) base-case run, list of retrospective runs,
#' and list of base-case and aspm runs.
#' @docType data
#' @format Individual list objects generated using \pkg{r4ss}:
#' \describe{
#'  \item{ss3phk}{output from Stock Synthesis as read by \code{\link[r4ss]{SS_output}()}}
#'  \item{retro.phk}{list of retrospective runs with \code{\link[r4ss]{SS_doRetro}()} as read by \code{\link[r4ss]{SSgetoutput}()}}
#'  \item{aspm.phk}{list of Stock Synthesis referece and aspm created with \code{\link[r4ss]{SSsummarize}()}}
#' }
#' @source 2020 Pacific hake/whiting Stock Synthesis assessment. Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting
#' Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada
#' @author Grandin, C.J., K.F. Johnson, A.M. Edwards, and A.M. Berger. (2020). Status of the Pacific Hake (whiting) stock in U.S. and Canadian waters in 2020. Prepared by the Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada. 273 p.
#' @examples
#' \dontrun{
#' data(pac.hke, package = "ss3diags")
#' 
#' # List the three objects returned by the line above
#' ls(pattern = "phk")
#' 
#' # The list of retrospective runs, which contains 8 lists, one for each peel
#' str(retro.phk, max.level = 1)
#' }
NULL



#' @name natl.sma
#' @title ICCAT 2017 North Atlantic Shortfin Mako Stock Synthesis model run 3
#' @description Information relevant to North Atlantic Shortfin Mako (SMA), including
#' Stock Synthesis (V3.30.15) reference run 3, list of retrospective runs,
#' and list of reference run 3 and aspm runs.
#' @docType data
#' @format Individual list objects generated using \pkg{r4ss}
#' \describe{
#'  \item{ss3sma}{output from Stock Synthesis as read by \code{\link[r4ss]{SS_output}()}}
#'  \item{retro.sma}{list of retrospective runs with \code{\link[r4ss]{SS_doRetro}()} as read by \code{\link[r4ss]{SSgetoutput}()}}
#'  \item{aspm.sma}{list of Stock Synthesis referece and aspm created with \code{\link[r4ss]{SSsummarize}()}}
#' }
#' @source ICCAT 2017. Report of the 2017 Shortfin Mako Assessment Meeting (Madrid, Spain 12–16 June 2017).Collect. Vol. Sci. Pap.
#'
#' @author Courtney, C., D., Carvalho, C. Winker, H., and LT Kell. (2020). Examples of Stock Synthesis diagnostic methods and results implemented for previously completed North Atlantic shortfin mako Stock Synthesis model runs. Collect. Vol. Sci. Pap. ICCAT, 76(10): 173-234.
#' @examples
#' \dontrun{
#' data(natl.sma, package = "ss3diags")
#' # List the three objects returned by the line above
#' ls(pattern = "sma")
#' }
NULL


#' 2020 Pacific Hake Stock Synthesis model output
#' 
#' Output from Stock Synthesis as read by \code{\link[r4ss]{SS_output}()}
#'
#' @docType data
#' 
#' @format List of Lobjects generated using \pkg{r4ss}:
#' 
#' \describe{
#'  \item{Data_File}{Data_File}
#'  \item{Control_File}{Control_File}
#'  \item{definitions}{definitions}
#'  \item{fleet_ID}{fleet_ID}
#'  \item{fleet_type}{fleet_type}
#'  \item{fleet_timing}{fleet_timing}
#'  \item{fleet_area}{fleet_area}
#'  \item{catch_units}{catch_units}
#'  \item{catch_se}{catch_se}
#'  \item{equ_catch_se}{equ_catch_se}
#'  \item{survey_units}{survey_units}
#'  \item{survey_error}{survey_error}
#'  \item{IsFishFleet}{IsFishFleet}
#'  \item{nfishfleets}{nfishfleets}
#'  \item{nfleets}{nfleets}
#'  \item{nsexes}{nsexes}
#'  \item{ngpatterns}{ngpatterns}
#'  \item{lbins}{lbins}
#'  \item{Lbin_method}{Lbin_method}
#'  \item{nlbins}{nlbins}
#'  \item{lbinspop}{lbinspop}
#'  \item{nlbinspop}{nlbinspop}
#'  \item{sizebinlist}{sizebinlist}
#'  \item{age_data_info}{age_data_info}
#'  \item{len_data_info}{len_data_info}
#'  \item{agebins}{agebins}
#'  \item{nagebins}{nagebins}
#'  \item{accuage}{accuage}
#'  \item{nareas}{nareas}
#'  \item{startyr}{startyr}
#'  \item{endyr}{endyr}
#'  \item{nseasons}{nseasons}
#'  \item{seasfracs}{seasfracs}
#'  \item{seasdurations}{seasdurations}
#'  \item{N_sub_seasons}{N_sub_seasons}
#'  \item{Spawn_month}{Spawn_month}
#'  \item{Spawn_seas}{Spawn_seas}
#'  \item{Spawn_timing_in_season}{Spawn_timing_in_season}
#'  \item{Retro_year}{Retro_year}
#'  \item{N_forecast_yrs}{N_forecast_yrs}
#'  \item{Empirical_wt_at_age}{Empirical_wt_at_age}
#'  \item{N_bio_patterns}{N_bio_patterns}
#'  \item{N_platoons}{N_platoons}
#'  \item{NatMort_option}{NatMort_option}
#'  \item{GrowthModel_option}{GrowthModel_option}
#'  \item{Maturity_option}{Maturity_option}
#'  \item{Fecundity_option}{Fecundity_option}
#'  \item{Start_from_par}{Start_from_par}
#'  \item{Do_all_priors}{Do_all_priors}
#'  \item{Use_softbound}{Use_softbound}
#'  \item{N_nudata}{N_nudata}
#'  \item{Max_phase}{Max_phase}
#'  \item{Current_phase}{Current_phase}
#'  \item{Jitter}{Jitter}
#'  \item{ALK_tolerance}{ALK_tolerance}
#'  \item{nforecastyears}{nforecastyears}
#'  \item{morph_indexing}{morph_indexing}
#'  \item{MGparmAdj}{MGparmAdj}
#'  \item{forecast_selectivity}{forecast_selectivity}
#'  \item{SelAgeAdj}{SelAgeAdj}
#'  \item{recruitment_dist}{recruitment_dist}
#'  \item{recruit}{recruit}
#'  \item{SPAWN_RECR_CURVE}{SPAWN_RECR_CURVE}
#'  \item{breakpoints_for_bias_adjustment_ramp}{breakpoints_for_bias_adjustment_ramp}
#'  \item{SpawnOutputUnits}{SpawnOutputUnits}
#'  \item{biology}{biology}
#'  \item{FecType}{FecType}
#'  \item{FecPar1name}{FecPar1name}
#'  \item{FecPar2name}{FecPar2name}
#'  \item{FecPar1}{FecPar1}
#'  \item{FecPar2}{FecPar2}
#'  \item{M_type}{M_type}
#'  \item{Natural_Mortality_Bmark}{Natural_Mortality_Bmark}
#'  \item{Natural_Mortality_endyr}{Natural_Mortality_endyr}
#'  \item{Growth_Parameters}{Growth_Parameters}
#'  \item{growthCVtype}{growthCVtype}
#'  \item{endgrowth}{endgrowth}
#'  \item{wtatage_switch}{wtatage_switch}
#'  \item{mean_body_wt}{mean_body_wt}
#'  \item{growthseries}{growthseries}
#'  \item{growthvaries}{growthvaries}
#'  \item{sizeselex}{sizeselex}
#'  \item{ageselex}{ageselex}
#'  \item{F_method}{F_method}
#'  \item{exploitation}{exploitation}
#'  \item{catch}{catch}
#'  \item{summary_age}{summary_age}
#'  \item{timeseries}{timeseries}
#'  \item{spawnseas}{spawnseas}
#'  \item{mainmorphs}{mainmorphs}
#'  \item{birthseas}{birthseas}
#'  \item{depletion_basis}{depletion_basis}
#'  \item{Bratio_denominator}{Bratio_denominator}
#'  \item{Bratio_label}{Bratio_label}
#'  \item{discard}{discard}
#'  \item{discard_type}{discard_type}
#'  \item{DF_discard}{DF_discard}
#'  \item{discard_spec}{discard_spec}
#'  \item{mnwgt}{mnwgt}
#'  \item{DF_mnwgt}{DF_mnwgt}
#'  \item{sprseries}{sprseries}
#'  \item{managementratiolabels}{managementratiolabels}
#'  \item{F_report_basis}{F_report_basis}
#'  \item{sprtarg}{sprtarg}
#'  \item{btarg}{btarg}
#'  \item{minbthresh}{minbthresh}
#'  \item{Kobe_warn}{Kobe_warn}
#'  \item{Kobe_MSY_basis}{Kobe_MSY_basis}
#'  \item{Kobe}{Kobe}
#'  \item{index_variance_tuning_check}{index_variance_tuning_check}
#'  \item{cpue}{cpue}
#'  \item{natage}{natage}
#'  \item{natage_annual_1_no_fishery}{natage_annual_1_no_fishery}
#'  \item{natage_annual_2_with_fishery}{natage_annual_2_with_fishery}
#'  \item{batage}{batage}
#'  \item{natlen}{natlen}
#'  \item{batlen}{batlen}
#'  \item{fatage}{fatage}
#'  \item{discard_at_age}{discard_at_age}
#'  \item{catage}{catage}
#'  \item{movement}{movement}
#'  \item{ALK}{ALK}
#'  \item{AAK}{AAK}
#'  \item{age_error_mean}{age_error_mean}
#'  \item{age_error_sd}{age_error_sd}
#'  \item{N_ageerror_defs}{N_ageerror_defs}
#'  \item{equil_yield}{equil_yield}
#'  \item{Z_at_age}{Z_at_age}
#'  \item{M_at_age}{M_at_age}
#'  \item{Z_by_area}{Z_by_area}
#'  \item{M_by_area}{M_by_area}
#'  \item{Dynamic_Bzero}{Dynamic_Bzero}
#'  \item{comp_data_exists}{comp_data_exists}
#'  \item{lendbase}{lendbase}
#'  \item{sizedbase}{sizedbase}
#'  \item{agedbase}{agedbase}
#'  \item{condbase}{condbase}
#'  \item{ghostagedbase}{ghostagedbase}
#'  \item{ghostcondbase}{ghostcondbase}
#'  \item{ghostlendbase}{ghostlendbase}
#'  \item{ladbase}{ladbase}
#'  \item{wadbase}{wadbase}
#'  \item{tagdbase1}{tagdbase1}
#'  \item{tagdbase2}{tagdbase2}
#'  \item{age_comp_fit_table}{age_comp_fit_table}
#'  \item{size_comp_fit_table}{size_comp_fit_table}
#'  \item{derived_quants}{derived_quants}
#'  \item{parameters}{parameters}
#'  \item{FleetNames}{FleetNames}
#'  \item{repfiletime}{repfiletime}
#'  \item{SRRtype}{SRRtype}
#'  \item{Pstar_sigma}{Pstar_sigma}
#'  \item{OFL_sigma}{OFL_sigma}
#'  \item{CoVar}{CoVar}
#'  \item{highcor}{highcor}
#'  \item{lowcor}{lowcor}
#'  \item{corstats}{corstats}
#'  \item{stdtable}{stdtable}
#'  \item{recruitpars}{recruitpars}
#'  \item{RecrDistpars}{RecrDistpars}
#'  \item{wtatage}{wtatage}
#'  \item{jitter_info}{jitter_info}
#'  \item{SS_version}{SS_version}
#'  \item{SS_versionshort}{SS_versionshort}
#'  \item{SS_versionNumeric}{SS_versionNumeric}
#'  \item{StartTime}{StartTime}
#'  \item{RunTime}{RunTime}
#'  \item{Files_used}{Files_used}
#'  \item{Nwarnings}{Nwarnings}
#'  \item{warnings}{warnings}
#'  \item{likelihoods_used}{likelihoods_used}
#'  \item{likelihoods_laplace}{likelihoods_laplace}
#'  \item{likelihoods_by_fleet}{likelihoods_by_fleet}
#'  \item{Parm_devs_detail}{Parm_devs_detail}
#'  \item{N_estimated_parameters}{N_estimated_parameters}
#'  \item{table_of_phases}{table_of_phases}
#'  \item{estimated_non_dev_parameters}{estimated_non_dev_parameters}
#'  \item{Dirichlet_Multinomial_pars}{Dirichlet_Multinomial_pars}
#'  \item{log_det_hessian}{log_det_hessian}
#'  \item{maximum_gradient_component}{maximum_gradient_component}
#'  \item{parameters_with_highest_gradients}{parameters_with_highest_gradients}
#'  \item{Length_Comp_Fit_Summary}{Length_Comp_Fit_Summary}
#'  \item{Age_Comp_Fit_Summary}{Age_Comp_Fit_Summary}
#'  \item{SBzero}{SBzero}
#'  \item{current_depletion}{current_depletion}
#'  \item{last_years_SPR}{last_years_SPR}
#'  \item{SPRratioLabel}{SPRratioLabel}
#'  \item{sigma_R_in}{sigma_R_in}
#'  \item{sigma_R_info}{sigma_R_info}
#'  \item{rmse_table}{rmse_table}
#'  \item{logfile}{logfile}
#'  \item{inputs}{inputs}
#' }
#' 
#' @source 2020 Pacific hake/whiting Stock Synthesis assessment. Joint Technical 
#' Committee of the U.S. and Canada Pacific Hake/Whiting Agreement, 
#' National Marine Fisheries Service and Fisheries and Oceans Canada
#' 
#' @author Grandin, C.J., K.F. Johnson, A.M. Edwards, and A.M. Berger. (2020). 
#' Status of the Pacific Hake (whiting) stock in U.S. and Canadian waters in 2020. 
#' Prepared by the Joint Technical Committee of the U.S. and Canada 
#' Pacific Hake/Whiting Agreement, National Marine Fisheries Service and 
#' Fisheries and Oceans Canada. 273 p.
#' 
"ss3phk"

#' 2020 Pacific Hake Retrospective Runs
#' 
#' List of retrospective runs with \code{\link[r4ss]{SS_doRetro}()} as read by \code{\link[r4ss]{SSgetoutput}()}
#'
#' @docType data
#' 
#' @format List of 8 retrospective runs generated using \pkg{r4ss}:
#' 
#' \describe{
#'  \item{replist1}{replist1}
#'  \item{replist2}{replist2}
#'  \item{replist3}{replist3}
#'  \item{replist4}{replist4}
#'  \item{replist5}{replist5}
#'  \item{replist6}{replist6}
#'  \item{replist7}{replist7}
#'  \item{replist8}{replist8}
#' }
#' 
#' @source 2020 Pacific hake/whiting Stock Synthesis assessment. Joint Technical 
#' Committee of the U.S. and Canada Pacific Hake/Whiting Agreement, 
#' National Marine Fisheries Service and Fisheries and Oceans Canada
#' 
#' @author Grandin, C.J., K.F. Johnson, A.M. Edwards, and A.M. Berger. (2020). 
#' Status of the Pacific Hake (whiting) stock in U.S. and Canadian waters in 2020. 
#' Prepared by the Joint Technical Committee of the U.S. and Canada 
#' Pacific Hake/Whiting Agreement, National Marine Fisheries Service and 
#' Fisheries and Oceans Canada. 273 p.
#' 
"retro.phk"

#' 2020 Pacific Hake ASPM Stock Synthesis Summation
#' 
#' List of Stock Synthesis references and aspm created with \code{\link[r4ss]{SSsummarize}()}
#'
#' @docType data
#' 
#' @format List of 54 objects generated using \pkg{r4ss}:
#' 
#' \describe{
#'  \item{n}{n}
#'  \item{npars}{npars}
#'  \item{modelnames}{modelnames}
#'  \item{maxgrad}{maxgrad}
#'  \item{nsexes}{nsexes}
#'  \item{startyrs}{startyrs}
#'  \item{endyrs}{endyrs}
#'  \item{pars}{pars}
#'  \item{parsSD}{parsSD}
#'  \item{parphases}{parphases}
#'  \item{quants}{quants}
#'  \item{quantsSD}{quantsSD}
#'  \item{likelihoods}{likelihoods}
#'  \item{likelambdas}{likelambdas}
#'  \item{likelihoods_by_fleet}{likelihoods_by_fleet}
#'  \item{SpawnBio}{SpawnBio}
#'  \item{SpawnBioSD}{SpawnBioSD}
#'  \item{SpawnBioLower}{SpawnBioLower}
#'  \item{SpawnBioUpper}{SpawnBioUpper}
#'  \item{Bratio}{Bratio}
#'  \item{BratioSD}{BratioSD}
#'  \item{BratioLower}{BratioLower}
#'  \item{BratioUpper}{BratioUpper}
#'  \item{SPRratio}{SPRratio}
#'  \item{SPRratioSD}{SPRratioSD}
#'  \item{SPRratioLower}{SPRratioLower}
#'  \item{SPRratioUpper}{SPRratioUpper}
#'  \item{SPRratioLabels}{SPRratioLabels}
#'  \item{Fvalue}{Fvalue}
#'  \item{FvalueSD}{FvalueSD}
#'  \item{FvalueLower}{FvalueLower}
#'  \item{FvalueUpper}{FvalueUpper}
#'  \item{FvalueLabels}{FvalueLabels}
#'  \item{sprtargs}{sprtargs}
#'  \item{btargs}{btargs}
#'  \item{minbthreshs}{minbthreshs}
#'  \item{recruits}{recruits}
#'  \item{recruitsSD}{recruitsSD}
#'  \item{recruitsLower}{recruitsLower}
#'  \item{recruitsUpper}{recruitsUpper}
#'  \item{recdevs}{recdevs}
#'  \item{recdevsSD}{recdevsSD}
#'  \item{recdevsLower}{recdevsLower}
#'  \item{recdevsUpper}{recdevsUpper}
#'  \item{growth}{growth}
#'  \item{sizesel}{sizesel}
#'  \item{agesel}{agesel}
#'  \item{indices}{indices}
#'  \item{InitAgeYrs}{InitAgeYrs}
#'  \item{lowerCI}{lowerCI}
#'  \item{upperCI}{upperCI}
#'  \item{SpawnOutputUnits}{SpawnOutputUnits}
#'  \item{FleetNames}{FleetNames}
#'  \item{mcmc}{mcmc} 
#' }
#' 
#' @source 2020 Pacific hake/whiting Stock Synthesis assessment. Joint Technical 
#' Committee of the U.S. and Canada Pacific Hake/Whiting Agreement, 
#' National Marine Fisheries Service and Fisheries and Oceans Canada
#' 
#' @author Grandin, C.J., K.F. Johnson, A.M. Edwards, and A.M. Berger. (2020). 
#' Status of the Pacific Hake (whiting) stock in U.S. and Canadian waters in 2020. 
#' Prepared by the Joint Technical Committee of the U.S. and Canada 
#' Pacific Hake/Whiting Agreement, National Marine Fisheries Service and 
#' Fisheries and Oceans Canada. 273 p.
#' 
"aspm.phk"

#' ICCAT 2017 North Atlantic Shortfin Mako ASPM Stock Synthesis model summarization
#' 
#' List of Stock Synthesis reference run #3 and aspm created with \code{\link[r4ss]{SSsummarize}()}
#' 
#' @docType data
#' 
#' @format Large list of 54 objects generated using \pkg{r4ss}
#' \describe{
#'  \item{n}{n}
#'  \item{npars}{number of pars}
#'  \item{modelnames}{modelnames}
#'  \item{maxgrad}{maxgrad} 
#'  \item{nsexes}{nsexes} 
#'  \item{startyrs}{startyrs} 
#'  \item{endyrs}{endyrs} 
#'  \item{pars}{pars} 
#'  \item{parsSD}{parsSD} 
#'  \item{parphases}{parphases} 
#'  \item{quants}{quants} 
#'  \item{quantsSD}{quantsSD} 
#'  \item{likelihoods}{likelihoods} 
#'  \item{likelambdas}{likelambdas} 
#'  \item{likelihoods_by_fleet}{likelihoods_by_fleet} 
#'  \item{SpawnBio}{SpawnBio} 
#'  \item{SpawnBioSD}{SpawnBioSD} 
#'  \item{SpawnBioLower}{SpawnBioLower} 
#'  \item{SpawnBioUpper}{SpawnBioUpper} 
#'  \item{Bratio}{Bratio} 
#'  \item{BratioSD}{BratioSD} 
#'  \item{BratioLower}{BratioLower} 
#'  \item{BratioUpper}{BratioUpper} 
#'  \item{SPRratio}{SPRratio} 
#'  \item{SPRratioSD}{SPRratioSD} 
#'  \item{SPRratioLower}{SPRratioLower} 
#'  \item{SPRratioUpper}{SPRratioUpper} 
#'  \item{SPRratioLabels}{SPRratioLabels} 
#'  \item{Fvalue}{Fvalue} 
#'  \item{FvalueSD}{FvalueSD} 
#'  \item{FvalueLower}{FvalueLower} 
#'  \item{FvalueUpper}{FvalueUpper} 
#'  \item{FvalueLabels}{FvalueLabels} 
#'  \item{sprtargs}{sprtargs} 
#'  \item{btargs}{btargs} 
#'  \item{minbthreshs}{minbthreshs} 
#'  \item{recruits}{recruits} 
#'  \item{recruitsSD}{recruitsSD} 
#'  \item{recruitsLower}{recruitsLower} 
#'  \item{recruitsUpper}{recruitsUpper} 
#'  \item{recdevs}{recdevs} 
#'  \item{recdevsSD}{recdevsSD} 
#'  \item{recdevsLower}{recdevsLower} 
#'  \item{recdevsUpper}{recdevsUpper} 
#'  \item{growth}{growth} 
#'  \item{sizesel}{sizesel} 
#'  \item{agesel}{agesel} 
#'  \item{indices}{indices} 
#'  \item{InitAgeYrs}{InitAgeYrs} 
#'  \item{lowerCI}{lowerCI} 
#'  \item{upperCI}{upperCI} 
#'  \item{SpawnOutputUnits}{SpawnOutputUnits} 
#'  \item{FleetNames}{FleetNames}
#'  \item{mcmc}{mcmc}
#' }
#'   
#' @source ICCAT 2017. Report of the 2017 Shortfin Mako Assessment Meeting (Madrid, Spain 12–16 June 2017).Collect. Vol. Sci. Pap.
#'
#' @author Courtney, C., D., Carvalho, C. Winker, H., and LT Kell. (2020). Examples of Stock Synthesis diagnostic methods and results implemented for previously completed North Atlantic shortfin mako Stock Synthesis model runs. Collect. Vol. Sci. Pap. ICCAT, 76(10): 173-234.
#' 
#'
"aspm.phk" 


#' ICCAT 2017 North Atlantic Shortfin Mako retrospective runs
#'
#' A list of retrospective runs with \code{\link[r4ss]{SS_doRetro}()} as read by 
#' \code{\link[r4ss]{SSgetoutput}()}
#' 
#' @docType data
#' 
#' @format Large List of 6 elements generated using \pkg{r4ss}
#' \describe{
#'  \item{replist1}{replist1}
#'  \item{replist2}{replist2}
#'  \item{replist3}{replist3}
#'  \item{replist4}{replist4}
#'  \item{replist5}{replist5}
#'  \item{replist6}{replist6}
#' }
#' 
#' @source ICCAT 2017. Report of the 2017 Shortfin Mako Assessment Meeting (Madrid, Spain 12–16 June 2017).Collect. Vol. Sci. Pap.
#'
#' @author Courtney, C., D., Carvalho, C. Winker, H., and LT Kell. (2020). Examples of Stock Synthesis diagnostic methods and results implemented for previously completed North Atlantic shortfin mako Stock Synthesis model runs. Collect. Vol. Sci. Pap. ICCAT, 76(10): 173-234.
#' 
"retro.sma"


#' ICCAT 2017 North Atlantic Shortfin Mako Stock Synthesis model run 3 output
#'
#' Stock Synthesis output read by \code{\link[r4ss]{SS_output}()}
#' 
#' @docType data
#' 
#' @format Large list of 184 objects generated using \pkg{r4ss}
#' \describe{
#'  \item{Data_File}{Data_File}
#'  \item{Control_File}{Control_File}
#'  \item{definitions}{definitions}
#'  \item{fleet_ID}{fleet_ID}
#'  \item{fleet_type}{fleet_type}
#'  \item{fleet_timing}{fleet_timing}
#'  \item{fleet_area}{fleet_area}
#'  \item{catch_units}{catch_units}
#'  \item{catch_se}{catch_se}
#'  \item{equ_catch_se}{equ_catch_se}
#'  \item{survey_units}{survey_units}
#'  \item{survey_error}{survey_error}
#'  \item{IsFishFleet}{IsFishFleet}
#'  \item{nfishfleets}{nfishfleets}
#'  \item{nfleets}{nfleets}
#'  \item{nsexes}{nsexes}
#'  \item{ngpatterns}{ngpatterns}
#'  \item{lbins}{lbins}
#'  \item{Lbin_method}{Lbin_method}
#'  \item{nlbins}{nlbins}
#'  \item{lbinspop}{lbinspop}
#'  \item{nlbinspop}{nlbinspop}
#'  \item{sizebinlist}{sizebinlist}
#'  \item{agebins}{agebins}
#'  \item{nagebins}{nagebins}
#'  \item{accuage}{accuage}
#'  \item{nareas}{nareas}
#'  \item{startyr}{startyr}
#'  \item{endyr}{endyr}
#'  \item{nseasons}{nseasons}
#'  \item{seasfracs}{seasfracs}
#'  \item{seasdurations}{seasdurations}
#'  \item{N_sub_seasons}{N_sub_seasons}
#'  \item{Spawn_month}{Spawn_month}
#'  \item{Spawn_seas}{Spawn_seas}
#'  \item{Spawn_timing_in_season}{Spawn_timing_in_season}
#'  \item{Retro_year}{Retro_year}
#'  \item{N_forecast_yrs}{N_forecast_yrs}
#'  \item{Empirical_wt_at_age}{Empirical_wt_at_age}
#'  \item{N_bio_patterns}{N_bio_patterns}
#'  \item{N_platoons}{N_platoons}
#'  \item{NatMort_option}{NatMort_option}
#'  \item{GrowthModel_option}{GrowthModel_option}
#'  \item{Maturity_option}{Maturity_option}
#'  \item{Fecundity_option}{Fecundity_option}
#'  \item{Start_from_par}{Start_from_par}
#'  \item{Do_all_priors}{Do_all_priors}
#'  \item{Use_softbound}{Use_softbound}
#'  \item{N_nudata}{N_nudata}
#'  \item{Max_phase}{Max_phase}
#'  \item{Current_phase}{Current_phase}
#'  \item{Jitter}{Jitter}
#'  \item{ALK_tolerance}{ALK_tolerance}
#'  \item{nforecastyears}{nforecastyears}
#'  \item{morph_indexing}{morph_indexing}
#'  \item{MGparmAdj}{MGparmAdj}
#'  \item{forecast_selectivity}{forecast_selectivity}
#'  \item{SelSizeAdj}{SelSizeAdj}
#'  \item{SelAgeAdj}{SelAgeAdj}
#'  \item{recruitment_dist}{recruitment_dist}
#'  \item{recruit}{recruit}
#'  \item{SPAWN_RECR_CURVE}{SPAWN_RECR_CURVE}
#'  \item{breakpoints_for_bias_adjustment_ramp}{breakpoints_for_bias_adjustment_ramp}
#'  \item{SpawnOutputUnits}{SpawnOutputUnits}
#'  \item{biology}{biology}
#'  \item{FecType}{FecType}
#'  \item{FecPar1name}{FecPar1name}
#'  \item{FecPar2name}{FecPar2name}
#'  \item{FecPar1}{FecPar1}
#'  \item{FecPar2}{FecPar2}
#'  \item{M_type}{M_type}
#'  \item{Natural_Mortality_Bmark}{Natural_Mortality_Bmark}
#'  \item{Natural_Mortality_endyr}{Natural_Mortality_endyr}
#'  \item{Growth_Parameters}{Growth_Parameters}
#'  \item{growthCVtype}{growthCVtype}
#'  \item{endgrowth}{endgrowth}
#'  \item{wtatage_switch}{wtatage_switch}
#'  \item{mean_body_wt}{mean_body_wt}
#'  \item{growthseries}{growthseries}
#'  \item{growthvaries}{growthvaries}
#'  \item{sizeselex}{sizeselex}
#'  \item{ageselex}{ageselex}
#'  \item{F_method}{F_method}
#'  \item{exploitation}{exploitation}
#'  \item{catch}{catch}
#'  \item{summary_age}{summary_age}
#'  \item{timeseries}{timeseries}
#'  \item{spawnseas}{spawnseas}
#'  \item{mainmorphs}{mainmorphs}
#'  \item{birthseas}{birthseas}
#'  \item{depletion_method}{depletion_method}
#'  \item{depletion_basis}{depletion_basis}
#'  \item{discard}{discard}
#'  \item{discard_type}{discard_type}
#'  \item{DF_discard}{DF_discard}
#'  \item{discard_spec}{discard_spec}
#'  \item{mnwgt}{mnwgt}
#'  \item{DF_mnwgt}{DF_mnwgt}
#'  \item{sprseries}{sprseries}
#'  \item{managementratiolabels}{managementratiolabels}
#'  \item{F_report_basis}{F_report_basis}
#'  \item{B_ratio_denominator}{B_ratio_denominator}
#'  \item{sprtarg}{sprtarg}
#'  \item{btarg}{btarg}
#'  \item{minbthresh}{minbthresh}
#'  \item{Kobe_warn}{Kobe_warn}
#'  \item{Kobe_MSY_basis}{Kobe_MSY_basis}
#'  \item{Kobe}{Kobe}
#'  \item{index_variance_tuning_check}{index_variance_tuning_check}
#'  \item{cpue}{cpue}
#'  \item{natage}{natage}
#'  \item{natage_annual_1_no_fishery}{natage_annual_1_no_fishery}
#'  \item{natage_annual_2_with_fishery}{natage_annual_2_with_fishery}
#'  \item{batage}{batage}
#'  \item{natlen}{natlen}
#'  \item{batlen}{batlen}
#'  \item{fatage}{fatage}
#'  \item{discard_at_age}{discard_at_age}
#'  \item{catage}{catage}
#'  \item{movement}{movement}
#'  \item{ALK}{ALK}
#'  \item{N_ageerror_defs}{N_ageerror_defs}
#'  \item{equil_yield}{equil_yield}
#'  \item{Z_at_age}{Z_at_age}
#'  \item{M_at_age}{M_at_age}
#'  \item{Dynamic_Bzero}{Dynamic_Bzero}
#'  \item{comp_data_exists}{comp_data_exists}
#'  \item{lendbase}{lendbase}
#'  \item{sizedbase}{sizedbase}
#'  \item{agedbase}{agedbase}
#'  \item{condbase}{condbase}
#'  \item{ghostagedbase}{ghostagedbase}
#'  \item{ghostcondbase}{ghostcondbase}
#'  \item{ghostlendbase}{ghostlendbase}
#'  \item{ladbase}{ladbase}
#'  \item{wadbase}{wadbase}
#'  \item{tagdbase1}{tagdbase1}
#'  \item{tagdbase2}{tagdbase2}
#'  \item{len_comp_fit_table}{len_comp_fit_table}
#'  \item{size_comp_fit_table}{size_comp_fit_table}
#'  \item{derived_quants}{derived_quants}
#'  \item{parameters}{parameters}
#'  \item{FleetNames}{FleetNames}
#'  \item{repfiletime}{repfiletime}
#'  \item{SRRtype}{SRRtype}
#'  \item{Pstar_sigma}{Pstar_sigma}
#'  \item{OFL_sigma}{OFL_sigma}
#'  \item{CoVar}{CoVar}
#'  \item{highcor}{highcor}
#'  \item{lowcor}{lowcor}
#'  \item{corstats}{corstats}
#'  \item{stdtable}{stdtable}
#'  \item{recruitpars}{recruitpars}
#'  \item{RecrDistpars}{RecrDistpars}
#'  \item{wtatage}{wtatage}
#'  \item{jitter_info}{jitter_info}
#'  \item{SS_version}{SS_version}
#'  \item{SS_versionshort}{SS_versionshort}
#'  \item{SS_versionNumeric}{SS_versionNumeric}
#'  \item{StartTime}{StartTime}
#'  \item{RunTime}{RunTime}
#'  \item{Files_used}{Files_used}
#'  \item{Nwarnings}{Nwarnings}
#'  \item{warnings}{warnings}
#'  \item{likelihoods_used}{likelihoods_used}
#'  \item{likelihoods_laplace}{likelihoods_laplace}
#'  \item{likelihoods_by_fleet}{likelihoods_by_fleet}
#'  \item{N_estimated_parameters}{N_estimated_parameters}
#'  \item{table_of_phases}{table_of_phases}
#'  \item{estimated_non_dev_parameters}{estimated_non_dev_parameters}
#'  \item{log_det_hessian}{log_det_hessian}
#'  \item{maximum_gradient_component}{maximum_gradient_component}
#'  \item{parameters_with_highest_gradients}{parameters_with_highest_gradients}
#'  \item{Length_Comp_Fit_Summary}{Length_Comp_Fit_Summary}
#'  \item{Age_Comp_Fit_Summary}{Age_Comp_Fit_Summary}
#'  \item{SBzero}{SBzero}
#'  \item{current_depletion}{current_depletion}
#'  \item{last_years_SPR}{last_years_SPR}
#'  \item{SPRratioLabel}{SPRratioLabel}
#'  \item{sigma_R_in}{sigma_R_in}
#'  \item{sigma_R_info}{sigma_R_info}
#'  \item{rmse_table}{rmse_table}
#'  \item{logfile}{logfile}
#'  \item{inputs}{inputs}
#' }
#' 
#' @source ICCAT 2017. Report of the 2017 Shortfin Mako Assessment Meeting (Madrid, Spain 12–16 June 2017).Collect. Vol. Sci. Pap.
#'
#' @author Courtney, C., D., Carvalho, C. Winker, H., and LT Kell. (2020). Examples of Stock Synthesis diagnostic methods and results implemented for previously completed North Atlantic shortfin mako Stock Synthesis model runs. Collect. Vol. Sci. Pap. ICCAT, 76(10): 173-234.
#' 
"ss3sma"