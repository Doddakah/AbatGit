*&---------------------------------------------------------------------*
*& Report Z_FIM_SA_EINVOICING_REP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT  z_fim_sad_einvoicing_rep_githu.

INCLUDE Z_FIM_SAD_EIN_GIT_TOP.
*INCLUDE z_fim_sad_einvoicing_rep_top.
INCLUDE Z_FIM_SAD_EIN_PRC_GITHUB.
*INCLUDE z_fim_sad_einvoicing_process.
*INCLUDE z_fim_sa_einvoicing_rep_top.

START-OF-SELECTION.

  CREATE OBJECT lc_obj_einvioce.

  lc_obj_einvioce->extract_data( EXPORTING it_bukrs = so_bukrs[]
                                           it_vkorg = so_vkorg[]
                                           it_auart = so_auart[]
                                           it_fkdat = so_fkdat[]
                                           it_aubel = so_aubel[]
                                           it_vbeln = so_vbeln[]
                                           it_kunag = so_kunag[]
                                           it_vstat = so_vstat[]
                                           it_aland = so_aland[]
                                           is_credit = p_credit
                                           is_puff   = p_puf ).
  PERFORM display_report.

*  lc_obj_einvioce->display_report( ).
