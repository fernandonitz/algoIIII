IDENTIFICATION DIVISION.
PROGRAM-ID. TP.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
	DECIMAL-POINT IS COMMA.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT SUC1 ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS suc1-estado.
	SELECT SUC2 ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS suc2-estado.
	SELECT SUC3 ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS suc3-estado.
	SELECT TIM ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS times-estado.
	SELECT SUCURSALES ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS sucursales-estado.
	SELECT PROFESORES ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS profesores-estado.
	SELECT TIPOSCLASE ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS tipos_clase-estado.
    SELECT MASTER ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS mae-estado.
DATA DIVISION.
FILE SECTION.

FD SUC1 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/suc1.txt".
01 reg_suc1.
	  03 suc1_num 						PIC X(5).
	  03 suc1_fecha.
	  	05 suc1_fecha_ano				PIC 9(4).
	  	05 suc1_fecha_mes				PIC 9(2).
	  	05 suc1_fecha_dia				PIC 9(2).
	  03 suc1_suc 						PIC X(3).
	  03 suc1_clase 					PIC X(4).
	  03 suc1_horas 					PIC 9(2)V99.
	  
FD SUC2 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/suc2.txt".
01 reg_suc2.
	  03 suc2_num 						PIC X(5).
	  03 suc2_fecha.
	  	05 suc2_fecha_ano				PIC 9(4).
	  	05 suc2_fecha_mes				PIC 9(2).
	  	05 suc2_fecha_dia				PIC 9(2).
	  03 suc2_suc 						PIC X(3).
	  03 suc2_clase 					PIC X(4).
	  03 suc2_horas 					PIC 9(2)V99.
	  
FD SUC3 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/suc3.txt".
01 reg_suc3.
	  03 suc3_num 						PIC X(5).
	  03 suc3_fecha.
	  	05 suc3_fecha_ano				PIC 9(4).
	  	05 suc3_fecha_mes				PIC 9(2).
	  	05 suc3_fecha_dia				PIC 9(2).
	  03 suc3_suc 						PIC X(3).
	  03 suc3_clase 					PIC X(4).
	  03 suc3_horas 					PIC 9(2)V99.
	  
FD PROFESORES LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/profesores.txt".
01 reg_profesores.
	  03 prof_num 						PIC X(5).
	  03 prof_dni 						PIC 9(8).
	  03 prof_nom 						PIC X(25).
	  03 prof_dire 						PIC X(20).
	  03 prof_tel 						PIC X(20).

FD TIM LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/times.txt".
01 reg_time.
	  03 tim_num 						PIC X(5).
	  03 tim_fecha.
	  	05 tim_fecha_ano				PIC 9(4).
	  	05 tim_fecha_mes				PIC 9(2).
	  	05 tim_fecha_dia				PIC 9(2).
	  03 tim_suc 						PIC X(3).
	  03 tim_clase 						PIC X(4).
	  03 tim_horas 						PIC 9(2)V99.

FD SUCURSALES LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/sucursales.txt".
01 reg_sucursal.
	  03 suc_suc 						PIC X(3).
	  03 suc_razon 						PIC 9(25).
	  03 suc_dire 						PIC X(20).
	  03 suc_tel 						PIC X(20).
	  03 suc_cuit 						PIC 9(11).

FD TIPOSCLASE LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/tiposClase.txt".
01 reg_tipclase.
	  03 tip_clase_suc 					PIC X(4).
	  03 tip_clase_razon 				PIC X(20).
	  03 tip_clase_dire 				PIC 9(5)V99.
	  
FD MASTER LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/mae.txt".
01 reg_mae 								PIC X(24).

WORKING-STORAGE SECTION.

	77  profesores-estado  				PIC XX VALUE "NO".
		88 profesores-estado_eof		VALUE "SI".
	77 	suc1-estado 					PIC XX VALUE "NO". 
		88 suc1-estado_eof 				VALUE "SI".
	77 	suc2-estado						PIC XX VALUE "NO". 
		88 suc2-estado_eof 				VALUE "SI".
	77 	suc3-estado 					PIC XX VALUE "NO".
		88 suc3-estado_eof 				VALUE "SI".
	77 	times-estado 					PIC XX VALUE "NO".
		88 times-estado_eof				VALUE "SI".
	77 	sucursales-estado 				PIC XX VALUE "NO". 
		88 sucursales-estado_eof 		VALUE "SI".
	77 	tipos_clase-estado 				PIC XX VALUE "NO". 
		88 tipos_clase-estado_eof 		VALUE "SI".
	77 	mae-estado 						PIC XX.
		88 mae-estado_ok 				VALUE "NO". 
		88 mae-estado_eof 				VALUE "SI".
	77  WB-FIN-ENTRADA 					PIC X(1) VALUE "N".
    	88 FIN-ENTRADA 					VALUE "S".

	77 FECHA8 							PIC 9(8).

	01 FECHA_HOY 						PIC X(8).
   	*> TOTALES PARCIALES:
   	01 TARIFA_TOT						PIC 9(11)V99 VALUE 0.
   	01 TARIFA_PROF 						PIC 9(10)V99 VALUE 0.
   	01 TARIFA_FECHA 					PIC 9(9)V99 VALUE 0.

	01 TOT_GRAL 						PIC 9(13) VALUE 0.
	01 TOT_X_PROF 						PIC 9(6) VALUE 0.
	01 TOT_X_FECHA 						PIC 9(6) VALUE 0.
	01 TOT_X_SUC 						PIC 9(6) VALUE 0.
	01 TOT_IMPR							PIC X(6).
	
	01 fin_suc1 						PIC XX   VALUE "NO".
	01 fin_suc2 						PIC XX   VALUE "NO".
	01 fin_suc3 						PIC XX   VALUE "NO".
	01 fin_times 						PIC XX   VALUE "NO".
	01 fin_sucur 						PIC XX   VALUE "NO".
	01 fin_tipCla 						PIC XX   VALUE "NO".

	01 archALeer 						PIC 9(1) VALUE 0.
	*> 1=suc1, 2=suc2, 3=suc3, 4=Times
	*> REGISTROS MINIMOS:
	01 prof_min							PIC X(5).
	01 prof_ant_min						PIC X(5).
 	01 fecha_min 						PIC X(8).
 	01 fecha_ant_min 					PIC X(8).
 	01 cant_horas_a_sumar				PIC 9(2)V99.

 	01 cont_titulo						PIC 9(2) VALUE 0.
 	01 tarifa 							PIC 9(11)V99 VALUE 0.

 	01 ano 								PIC X(4).
 	01 I 								PIC 9(3).
 	01 II 								PIC 9(4).
 	*> vector de sucursales 
	01 v_sucursales.
	  03 v_sucursal OCCURS 3 TIMES ascending key is v_suc_num indexed by ind.
	  	05 v_suc_num 					PIC X(3).
	  	05 v_suc_razon 					PIC 9(25).
	  	05 v_suc_dire 					PIC X(20).
	  	05 v_suc_tel 					PIC X(20).
	  	05 v_suc_cuit 					PIC 9(11).

	01 aux_sucursal.
	  03 aux_suc_num 					PIC X(3).
	  03 aux_suc_razon 					PIC 9(25).
	  03 aux_suc_dire 					PIC X(20).
	  03 aux_suc_tel 					PIC X(20).
	  03 aux_suc_cuit 					PIC 9(11).

	01 matriz_horas.
	  03 v_suc OCCURS 3 TIMES ascending key is m_suc indexed by suci.
	  	05 v_ano OCCURS 5 TIMES ascending key is m_ano indexed by anoi.
	  	   07 v_mes OCCURS 12 TIMES ascending key is m_mes indexed by mesi.
	  	   	  09 m_suc					PIC X(3).
	  	   	  09 m_ano					PIC X(4).
	  	   	  09 m_mes					PIC X(2).
	  	   	  09 m_horas 				PIC 9(3) VALUE 0.

	01 tot_ano.
	  03 v_suc_ano OCCURS 3 TIMES ascending key is m2_suc indexed by sucii.
	    05 v_tot_ano OCCURS 5 TIMES ascending key is m2_ano indexed by anoii.
	  	  07 m2_suc						PIC X(3).
	  	  07 m2_ano 					PIC X(4).
	  	  07 m2_horas 					PIC 9(4) VALUE 0.

	01 tot_mes.
	  03 v_tot_mes OCCURS 12 TIMES ascending key is m3_mes indexed by mesii.
	  	05 m3_mes						PIC X(2).
	  	05 m3_horas						PIC 9(3) VALUE 0.

	01 tot_tot_mat 						PIC 9(4) VALUE 0.

	01 v_tipos_clase.
	  03 v_tipo_clase OCCURS 50 TIMES ascending key is v_tip_clase_num indexed by indi.
	  	05 v_tip_clase_num 				PIC X(4).
	  	05 v_tip_clase_desc 			PIC X(20).
	  	05 v_tip_clase_tarifa			PIC 9(5)V99. 	

	01 aux_tipo_clase.
	  03 aux_tip_clase_num 				PIC X(4).
	  03 aux_tip_clase_desc 			PIC X(20).
      03 aux_tip_clase_tarifa			PIC 9(5)V99. 	


	01 cant_hojas						PIC 9(3) VALUE 1.
	01 fecha_hoy_a 						PIC X(10) VALUE "12/12/1234".

	01 linea_impr 						PIC X(100).
	01 sucursal_impr					PIC X(3) VALUE "999".


	01 encabezado.
	  03 fechaHoy 						PIC X(5) VALUE "Fecha".
	  03 nada 							PIC X(1) VALUE " ".
	  03 enc_fecha_hoy					PIC X(10) VALUE	"XX/XX/XXXX".
	  03 nada 							PIC X(45) VALUE ALL " ".
	  03 hoja 							PIC X(4) VALUE "Hoja".
	  03 nada3 							PIC X(1) VALUE " ".
	  03 enc_num_hoja					PIC X(3) VALUE "XXX".

	01 titulo.
	  03 nada 							PIC X(20) VALUE ALL " ".
	  03 tit 							PIC X(26) VALUE "Listado de horas aplicadas".

	01 titulo2. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 tit 							PIC X(75) VALUE "Listado de Estadístico de Horas aplicadas por año y mes".


	01 titulo_prof. 						
	  03 prof 							PIC X(9) VALUE "Profesor:".
	  03 nada 							PIC X(1) VALUE " ". 
	  03 tit_num_prof					PIC X(5) VALUE "XXXXX".
	  03 nada 							PIC X(10) VALUE ALL " ".
	  03 nom_prof 						PIC X(8) VALUE "Nombre: ".
	  03 tit_nom_prof					PIC X(25) VALUE ALL " ".

	01 culumnas_mat.
	  03 nada 							PIC X(5) VALUE ALL " ".
	  03 fecha 							PIC X(5) VALUE "Fecha".
	  03 nada 							PIC X(4) VALUE ALL " ".
	  03 sucurs 						PIC X(8) VALUE "Sucursal".
	  03 nada 							PIC X(8) VALUE ALL " ".
	  03 tip_cla 						PIC X(13) VALUE "Tipo de clase".
	  03 nada 							PIC X(7) VALUE ALL " ".
	  03 tarif 							PIC X(6) VALUE "Tarifa".
  	  03 nada 							PIC X(4) VALUE ALL " ".
	  03 horas 							PIC X(5) VALUE "Horas".
	  03 nada 							PIC X(4) VALUE ALL " ".
	  03 impor 							PIC X(7) VALUE "Importe".

	01 culumna_mat2.
	  03 nada 							PIC X(4) VALUE ALL " ". 
	  03 surcursal 						PIC X(8) VALUE "SUCURSAL".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 ano_p 							PIC X(3) VALUE "ANO".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 ene 							PIC X(3) VALUE "ENE".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 feb 							PIC X(3) VALUE "FEB".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 mar 							PIC X(3) VALUE "MAR".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 abr 							PIC X(3) VALUE "ABR".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 may 							PIC X(3) VALUE "MAY".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 jun 							PIC X(3) VALUE "JUN".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 jul 							PIC X(3) VALUE "JUL".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 ago 							PIC X(3) VALUE "AGO".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 sep 							PIC X(3) VALUE "SEP".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 oct 							PIC X(3) VALUE "OCT".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 nov 							PIC X(3) VALUE "NOV".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 dic 							PIC X(3) VALUE "DIC".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 tot 							PIC X(5) VALUE "TOTAL".

	01 reg_mat_horas_con_suc.
	  03 nada 							PIC X(6) VALUE ALL " ". 
	  03 reg_mat1_suc 					PIC X(5).
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_ano_p 					PIC X(4).
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_ene 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_feb 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_mar 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_abr 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_may 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_jun 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_jul 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_ago 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_sep 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_oct 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_nov 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_dic 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat1_tot 					PIC X(5) VALUE ALL "0".


	01 reg_mat_horas_sin_suc.
	  03 nada 							PIC X(13) VALUE ALL " ".
	  03 reg_mat2_ano_p 					PIC X(4).
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_ene 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_feb 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_mar 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_abr 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_may 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_jun 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_jul 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_ago 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_sep 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_oct 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_nov 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_dic 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat2_tot 					PIC X(5) VALUE ALL "0".

	01 reg_mat_horas_tot.
	  03 tot 							PIC X(7) VALUE "Totales".
	  03 nada 							PIC X(12) VALUE ALL " ".
	  03 reg_mat3_ene 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_feb 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_mar 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_abr 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_may 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_jun 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_jul 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_ago 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_sep 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_oct 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_nov 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_dic 					PIC X(3) VALUE ALL "0".
	  03 nada  							PIC X(2) VALUE ALL " ".
	  03 reg_mat3_tot 					PIC X(5) VALUE ALL "0".

	01 linea.
	  03 guiones 						PIC X(80) VALUE ALL "-".

	01 reg1_con_fecha. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg1_fecha   					PIC X(10) VALUE "XX/XX/XXXX".
	  03 nada 							PIC X(4) VALUE ALL " ".
	  03 reg1_suc 						PIC X(3) VALUE "999".
	  03 nada 							PIC X(7) VALUE ALL " ".
	  03 reg1_tip_cla					PIC X(20) VALUE ALL "X".
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 reg1_tarifa 					PIC 9(6)V99.
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 reg1_horas 					PIC 9(2)V99.
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 reg1_impor						PIC 9(8)V99.

	01 reg2_sin_fecha. 
	  03 nada 							PIC X(16) VALUE ALL " ".
	  03 reg2_suc 						PIC X(3) VALUE "999".
	  03 nada 							PIC X(7) VALUE ALL " ".
	  03 reg2_tip_cla					PIC X(20) VALUE ALL "X".
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 reg2_tarifa 					PIC 9(6)V99.
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 reg2_horas 					PIC 9(2)V99.
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 reg2_impor						PIC 9(8)V99.

	01 separador_tot.
	  03 nada 							PIC X(60) VALUE ALL " ".
	  03 sep 							PIC X(5) VALUE ALL "-".
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 sep 							PIC X(10) VALUE ALL "-".
	
	01 separador2.
	  03 nada 							PIC X(84) VALUE ALL "-".

	01 reg3_tot_fecha. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg3_tit 						PIC X(17) VALUE "Totales por fecha".
	  03 nada 							PIC X(40) VALUE ALL " ".
	  03 reg3_horas 					PIC 9(3)V99.
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg3_impor						PIC 9(9)V99..

	01 reg4_tot_prof. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg4_tit 						PIC X(20) VALUE "Totales por profesor".
	  03 nada 							PIC X(36) VALUE ALL " ".
	  03 reg4_horas 					PIC 9(4)V99.
	  03 nada 							PIC X(1) VALUE ALL " ".
	  03 reg4_impor						PIC 9(10)V99.

	01 reg5_tot_gral. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg5_tit 						PIC X(13) VALUE "Total general".
	  03 nada 							PIC X(50) VALUE ALL " ".
	  03 reg5_impor						PIC 9(11)V99.

	01  W-FECHA-HORA-SISTEMA  			PIC S9(15) VALUE   0.
	01  W-ANNO-SISTEMA        			PIC S9(8) VALUE   0.
	01  W-MES-SISTEMA        			PIC S9(8) VALUE   0.
	01  W-DIA-SISTEMA         			PIC S9(8) VALUE   0.

	01  W-HORA-SISTEMA.                                   
	    03  W-HH-SISTEMA      			PIC 9(2) VALUE    0.
	    03  FILLER            			PIC X VALUE  '.'.
	    03  W-MM-SISTEMA      			PIC 9(2) VALUE    0.
	    03  FILLER            			PIC X VALUE  '.'.
	    03  W-SS-SISTEMA      			PIC 9(2) VALUE    0. 

	01  W-FECHA-SISTEMA.                                  
	    03  W-ANNO            			PIC 9(4) VALUE    0.
	    03  FILLER            			PIC X VALUE  '-'.
	    03  W-MES             			PIC 9(2) VALUE    0.
	    03  FILLER            			PIC X VALUE  '-'.
	    03  W-DIA             			PIC 9(2) VALUE    0.

	01  W-TIMESTAMP.                                        
	    03  W-FECHA-SYS       			PIC X(10) VALUE SPACES.
	    03  FILLER            			PIC X(1) VALUE '-'.   
	    03  W-HORA-SYS        			PIC X(8) VALUE SPACES.
	    03  FILLER            			PIC X(1) VALUE '-'.   
	    03  W-MILISEG-SYS     			PIC 9(6) VALUE 0. 
	
	01 l 								PIC 9(2) VALUE 1.
	01 m 								PIC 9(2) VALUE 1.
	01 ano_impr							PIC X(4) VALUE ALL " ".					
PROCEDURE DIVISION.

*> CICLO PPAL--------------------------------------------------------------
   *> ACCEPT FECHA8 FROM CENTURY-DATE.
	PERFORM 1_ABRO_ARCHIVOS.
	PERFORM 2_LEO_ARCHIVOS.
	PERFORM 3_CICLO_SUCURSALES UNTIL sucursales-estado_eof.
	PERFORM 4_CICLO_TIPOS_CLASE UNTIL tipos_clase-estado_eof.
	PERFORM 5_CICLO_ARCHIVOS UNTIL fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI".
	PERFORM 7_CIERRO_ARCHIVOS.
	PERFORM ARMAR_DISPLAY_GRAL.
	MOVE TOT_GRAL TO TOT_IMPR.
	PERFORM 6_IMPRIMO_MATRIZ.
	STOP RUN.   


*> --------------------------------------------------------------

1_ABRO_ARCHIVOS.
	OPEN INPUT SUC1.
	OPEN INPUT SUC2.
	OPEN INPUT SUC3.
	OPEN INPUT TIM.
	OPEN INPUT SUCURSALES.
	OPEN INPUT TIPOSCLASE.
	OPEN INPUT PROFESORES.
	OPEN OUTPUT MASTER.

2_LEO_ARCHIVOS.
	READ suc1 AT END MOVE "SI" TO fin_suc1.
	READ suc2 AT END MOVE "SI" TO fin_suc2.
	READ suc3 AT END MOVE "SI" TO fin_suc3.
	READ TIM  AT END MOVE "SI" TO fin_times.

CALCULO_ANT_FECHA_MIN.
	*> inicializo prof_ant sabiendo que hay un registro q la va a llenar 
	MOVE "NNNNNNNN" TO fecha_ant_min.
	*> caso 1º, ventaja que nadie la pudo inicializar antes
	IF (fin_suc1 IS = "NO" AND prof_ant_min IS = suc1_num)THEN
		MOVE suc1_fecha TO fecha_ant_min
		MOVE 1 TO archALeer		
	END-IF.
	*> caso 2º, completo
	IF (fin_suc2 IS = "NO" AND prof_ant_min IS = suc2_num AND (fecha_ant_min IS = "NNNNN" OR fecha_ant_min IS > suc2_fecha))THEN
		MOVE suc2_fecha TO fecha_ant_min
		MOVE 2 TO archALeer				
	END-IF.
	*> caso 3º, completo
	IF (fin_suc3 IS = "NO" AND prof_ant_min IS = suc3_num AND (fecha_ant_min IS = "NNNNN" OR fecha_ant_min IS > suc3_fecha))THEN
		MOVE suc3_fecha TO fecha_ant_min		
		MOVE 3 TO archALeer		
	END-IF.
    *> caso 4º, completo
	IF (fin_times IS = "NO" AND prof_ant_min IS = tim_num AND (fecha_ant_min IS = "NNNNN" OR fecha_ant_min IS > tim_fecha))THEN
		MOVE tim_fecha TO fecha_ant_min		
		MOVE 4 TO archALeer		
	END-IF.

3_CICLO_SUCURSALES.
*> se trae a memoria el archivo de sucursales con su respectivo formato y de ser necesario su indice
	READ sucursales AT END MOVE "SI" TO sucursales-estado.
	MOVE suc_suc TO ind.
	MOVE reg_sucursal TO v_sucursal(ind).

4_CICLO_TIPOS_CLASE.
*> se trae a memoria el archivo de tipos_clase con su respectivo formato y de ser necesario su indice
	READ tiposclase AT END MOVE "SI" TO tipos_clase-estado.
	MOVE tip_clase_suc TO indi.
	MOVE reg_tipclase TO v_tipo_clase(indi).


5_CICLO_ARCHIVOS.
	PERFORM 51_OBTENER_REG_MIN_PROF.
	PERFORM OBTENER_INFO_PROF UNTIL prof_num IS = prof_ant_min.
	MOVE 0 TO TOT_X_PROF.
	MOVE 0 TO TARIFA_PROF.	
	MOVE 0  TO cont_titulo.
	PERFORM ARMAR_ENCABEZADO.
	MOVE 0 TO cont_titulo.
	PERFORM 52_CICLO_PROFESORES UNTIL 
	((fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI") 
	or (prof_ant_min IS NOT = suc1_num and prof_ant_min IS NOT = suc2_num and prof_ant_min IS NOT = suc3_num and prof_ant_min IS NOT = tim_num)).
	PERFORM 53_ESCRIBO_TOT_PROF.
	PERFORM 54_IMPRIMO_TOT_PROF.

6_IMPRIMO_MATRIZ.
*> se debera leer toda la matriz (punto b) y mostrarla en el formato del enunciado 
*> lmo
	DISPLAY " ".
	DISPLAY encabezado.
	DISPLAY titulo2.
	DISPLAY " ".
	DISPLAY culumna_mat2.
	DISPLAY separador2.
	PERFORM X_suc 3 TIMES.

	move tot_tot_mat to reg_mat3_tot.

	move 1 to mesii.
	move m3_horas(mesii) to reg_mat3_ene.
	move 2 to mesii.
	move m3_horas(mesii) to reg_mat3_feb.
	move 3 to mesii.
	move m3_horas(mesii) to reg_mat3_mar.
	move 4 to mesii.
	move m3_horas(mesii) to reg_mat3_abr.
	move 5 to mesii.
	move m3_horas(mesii) to reg_mat3_may.
	move 6 to mesii.
	move m3_horas(mesii) to reg_mat3_jun.
	move 7 to mesii.
	move m3_horas(mesii) to reg_mat3_jul.
	move 8 to mesii.
	move m3_horas(mesii) to reg_mat3_ago.
	move 9 to mesii.
	move m3_horas(mesii) to reg_mat3_sep.
	move 10 to mesii.
	move m3_horas(mesii) to reg_mat3_oct.
	move 11 to mesii.
	move m3_horas(mesii) to reg_mat3_nov.
	move 12 to mesii.
	move m3_horas(mesii) to reg_mat3_dic. 
	DISPLAY reg_mat_horas_tot.

X_Suc.
	move 1 to m.
	move l to reg_mat1_suc.
	PERFORM LLENAR_ANO.
	move ano_impr to reg_mat1_ano_p.
	move l to suci.
	move m to anoi.
 
 	move 1 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_ene. 
	move 2 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_feb. 
	move 3 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_mar. 
	move 4 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_abr. 
	move 5 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_may. 
	move 6 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_jun. 
	move 7 to mesi.	
	move m_horas(suci,anoi,mesi) to reg_mat1_jul. 
	move 8 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_ago. 
	move 9 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_sep. 
	move 10 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_oct. 
	move 11 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_nov. 
	move 12 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat1_dic. 
	move anoi to anoii.
	move suci to sucii.
	move m2_horas(sucii,anoii) to reg_mat1_tot.

	DISPLAY reg_mat_horas_con_suc.
	MOVE 2 TO m. 
	PERFORM X_ano 4 TIMES.	
	ADD 1 TO l.
	DISPLAY " ".
X_ano.

	PERFORM LLENAR_ANO.
	move ano_impr to reg_mat2_ano_p.
	move l to suci.
	move m to anoi.
	move 1 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_ene. 
	move 2 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_feb. 
	move 3 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_mar. 
	move 4 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_abr. 
	move 5 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_may. 
	move 6 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_jun. 
	move 7 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_jul. 
	move 8 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_ago. 
	move 9 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_sep. 
	move 10 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_oct. 
	move 11 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_nov. 
	move 12 to mesi.
	move m_horas(suci,anoi,mesi) to reg_mat2_dic. 
	move anoi to anoii.
	move suci to sucii.
	move m2_horas(sucii,anoii) to reg_mat2_tot.

	DISPLAY reg_mat_horas_sin_suc.
	ADD 1 TO m.

7_CIERRO_ARCHIVOS.
	CLOSE SUC1.
	CLOSE SUC2.
	CLOSE SUC3.
	CLOSE TIM.
	CLOSE SUCURSALES.
	CLOSE TIPOSCLASE.
	CLOSE PROFESORES.
	CLOSE MASTER.

*> CICLO POR PROFESOR--------------------------------------------------------------

51_OBTENER_REG_MIN_PROF.
	*> inicializo prof_min sabiendo que hay un registro q la va a llenar 
	MOVE "NNNNN" TO prof_ant_min.
	*> caso 1º, ventaja que nadie la pudo inicializar antes
	IF (fin_suc1 IS not = "SI")THEN
		MOVE suc1_num TO prof_ant_min
	END-IF.
	*> caso 2º, completo
	IF (fin_suc2 IS not = "SI" AND (prof_ant_min IS = "NNNNN" OR prof_ant_min IS > suc2_num))THEN
		MOVE suc2_num TO prof_ant_min
	END-IF.
	*> caso 3º, completo
	IF (fin_suc3 IS not = "SI" AND (prof_ant_min IS = "NNNNN" OR prof_ant_min IS > suc3_num))THEN
		MOVE suc3_num TO prof_ant_min
	END-IF.
    *> caso 4º, completo
	IF (fin_times IS not = "SI" AND (prof_ant_min IS = "NNNNN" OR prof_ant_min IS > tim_num))THEN
		MOVE tim_num TO prof_ant_min
	END-IF.

52_CICLO_PROFESORES.
	PERFORM ARMAR_DISPLAY_FECHA_ANT
	PERFORM CALCULO_ANT_FECHA_MIN.
	MOVE 0 TO cont_titulo.
	MOVE 0 TO TOT_X_FECHA.
	MOVE 0 TO TARIFA_FECHA.
	PERFORM 522_CICLO_FECHA UNTIL 
	((fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI") 
	or (prof_ant_min IS NOT = suc1_num and prof_ant_min IS NOT = suc2_num and prof_ant_min IS NOT = suc3_num and prof_ant_min IS NOT = tim_num)
	or (fecha_ant_min IS NOT = suc1_fecha and fecha_ant_min IS NOT = suc2_fecha and fecha_ant_min IS NOT = suc3_fecha and fecha_ant_min IS NOT = tim_fecha)).
*> corte cuando eof de todos los archivos: EOF suc1 and EOF suc2 and EOF suc3 and EOF times and Prof_ant != Prof_act and Fecha_ant != Fecha_act
	PERFORM 523_ESCRIBO_TOT_FECHA.

53_ESCRIBO_TOT_PROF.
*> se debe escribir en el archivo master, el total por profesor como indica el enunciado
	PERFORM ARMAR_DISPLAY_PROF.

54_IMPRIMO_TOT_PROF.
*> se debe imprimir por pantalla (como dice el enunciado el total por prof, hasheando si fuese el caso al vector de sucursales y al vector de tipos_clase)

*> CICLO POR FECHA--------------------------------------------------------------

521_OBTENER_REG_MIN.
*> se debe inicializar el reg_min con el minimo de entre: reg_suc1, reg_suc2, 
*>reg_suc3, reg_times (comparando por numero de profesor, fecha y sucursal) para
*> dejar en el fecha_anterior el min y ademas ya comparar con ese minimo. Por lo 
*>tanto, ademas, se debera dejar guardado en el arch_min el archivo de donde se saco 
*>este registro minimo.

	MOVE "NNNNN" TO prof_min.
	MOVE "NNNNNNNN" TO fecha_min.
	*> caso 1º, ventaja que nadie la pudo inicializar antes
	IF (fin_suc1 IS not = "SI")THEN
		MOVE suc1_num TO prof_min
		MOVE suc1_fecha TO fecha_min
		MOVE 1 TO archALeer
	END-IF.
	*> caso 2º, completo
	IF (fin_suc2 IS not = "SI" AND prof_min IS = "NNNNN")THEN
		MOVE suc2_num TO prof_min
		MOVE suc2_fecha TO fecha_min
		MOVE 2 TO archALeer		
	ELSE IF (fin_suc2 IS not = "SI" AND suc2_num IS < prof_min) THEN 	
		MOVE suc2_num TO prof_min
		MOVE suc2_fecha TO fecha_min
		MOVE 2 TO archALeer		
	ELSE IF (fin_suc2 IS not = "SI" AND suc2_num IS = prof_min AND suc2_fecha IS < fecha_min) THEN 	
		MOVE suc2_num TO prof_min
		MOVE suc2_fecha TO fecha_min
		MOVE 2 TO archALeer		
	END-IF.
	*> caso 3º, completo
	IF (fin_suc3 IS not = "SI" AND prof_min IS = "NNNNN")THEN
		MOVE suc3_num TO prof_min
		MOVE suc3_fecha TO fecha_min
		MOVE 3 TO archALeer		
	ELSE IF (fin_suc3 IS not = "SI" AND suc3_num IS < prof_min) THEN 	
		MOVE suc3_num TO prof_min
		MOVE suc3_fecha TO fecha_min
		MOVE 3 TO archALeer
	ELSE IF (fin_suc3 IS not = "SI" AND suc3_num IS = prof_min AND suc3_fecha IS < fecha_min) THEN 	
		MOVE suc3_num TO prof_min
		MOVE suc3_fecha TO fecha_min
		MOVE 3 TO archALeer
	END-IF.
    *> caso 4º, completo
	IF (fin_times IS not = "SI" AND prof_min IS = "NNNNN")THEN
		MOVE tim_num TO prof_min
		MOVE tim_fecha TO fecha_min
		MOVE 4 TO archALeer
	ELSE IF (fin_times IS not = "SI" AND tim_num IS < prof_min) THEN 	
		MOVE tim_num TO prof_min
		MOVE tim_fecha TO fecha_min
		MOVE 4 TO archALeer
	ELSE IF (fin_times IS not = "SI" AND tim_num IS = prof_min AND tim_fecha IS < fecha_min) THEN 	
		MOVE tim_num TO prof_min
		MOVE tim_fecha TO fecha_min
		MOVE 4 TO archALeer
	END-IF.

522_CICLO_FECHA.
	IF (cont_titulo IS > 60) THEN
		PERFORM ARMAR_ENCABEZADO
	END-IF.
	PERFORM CALCULO_ANT_FECHA_MIN.
	PERFORM 5221_SUMAR_TOTALES.
	PERFORM 5222_SUMAR_EN_MATRIZ.
	PERFORM 5223_ESCRIBO_MOV.
	PERFORM 5224_LEO_ARCH_MIN.
	PERFORM 5225_BLOQUEO.
	ADD 1 TO cont_titulo.

523_ESCRIBO_TOT_FECHA.
*> se debe escribir en el archivo master, el total por fecha como indica el enunciado
	PERFORM ARMAR_DISPLAY_FECHA_DESP.

*> CICLO IND--------------------------------------------------------------

5221_SUMAR_TOTALES.
    IF (archALeer IS = 1 and fin_suc1 IS NOT = "SI")THEN
		MOVE suc1_horas TO cant_horas_a_sumar
	    MOVE suc1_suc TO sucursal_impr

	    MOVE suc1_suc TO ind
	    MOVE suc1_clase TO indi
	    MOVE suc1_fecha_mes TO mesi
	    MOVE suc1_fecha_ano TO ano
	    PERFORM INSERTAR_ANO
	    MOVE suc1_suc TO suci
    END-IF.
	
    IF (archALeer IS = 2 and fin_suc2 IS NOT = "SI")THEN
		MOVE suc2_horas TO cant_horas_a_sumar
	    MOVE suc2_suc TO sucursal_impr

	    MOVE suc2_suc TO ind
	    MOVE suc2_clase TO indi
	    MOVE suc2_fecha_mes TO mesi
	    MOVE suc2_fecha_ano TO ano
	    PERFORM INSERTAR_ANO
	    MOVE suc2_suc TO suci
    END-IF.
	
    IF (archALeer IS = 3 and fin_suc3 IS NOT = "SI")THEN
   		MOVE suc3_horas TO cant_horas_a_sumar
   		MOVE suc3_suc TO sucursal_impr

	    MOVE suc3_suc TO ind
	    MOVE suc3_clase TO indi
	    MOVE suc3_fecha_mes TO mesi
	    MOVE suc3_fecha_ano TO ano
	    PERFORM INSERTAR_ANO
	    MOVE suc3_suc TO suci
    END-IF.
	
    IF (archALeer IS = 4 and fin_times IS NOT = "SI")THEN
		MOVE tim_horas TO cant_horas_a_sumar
		MOVE tim_suc TO sucursal_impr

	    MOVE tim_suc TO ind
	    MOVE tim_clase TO indi
	    MOVE tim_fecha_mes TO mesi
	    MOVE tim_fecha_ano TO ano
  	    PERFORM INSERTAR_ANO
	    MOVE tim_suc TO suci
    END-IF.

	ADD cant_horas_a_sumar TO TOT_GRAL.
	ADD cant_horas_a_sumar TO TOT_X_PROF.
	ADD cant_horas_a_sumar TO TOT_X_FECHA.
	ADD cant_horas_a_sumar TO TOT_X_SUC.

    PERFORM ARMAR_DISPLAY_FECHA.

5222_SUMAR_EN_MATRIZ.
	MOVE anoi TO anoii.
	MOVE mesi TO mesii.
	MOVE suci TO sucii.
	ADD cant_horas_a_sumar TO m_horas(suci,anoi,mesi).
	ADD cant_horas_a_sumar TO m2_horas(sucii,anoii).
	ADD cant_horas_a_sumar TO m3_horas(mesii).
	ADD cant_horas_a_sumar TO tot_tot_mat.

5223_ESCRIBO_MOV.
	
    IF (archALeer IS = 1 and fin_suc1 IS NOT = "SI")THEN
	    MOVE reg_suc1 to reg_mae
		WRITE reg_mae
    END-IF.
	
    IF (archALeer IS = 2 and fin_suc2 IS NOT = "SI")THEN
	    MOVE reg_suc2 to reg_mae
		WRITE reg_mae
    END-IF.
	
    IF (archALeer IS = 3 and fin_suc3 IS NOT = "SI")THEN
	    MOVE reg_suc3 to reg_mae
		WRITE reg_mae
    END-IF.
	
    IF (archALeer IS = 4 and fin_times IS NOT = "SI")THEN
	    MOVE reg_time to reg_mae
		WRITE reg_mae
    END-IF.
	
*> se debe escribir el movimiento individual como lo dice el enunciado

5224_LEO_ARCH_MIN.
	IF (archALeer IS = 1)THEN
		READ suc1 AT END MOVE "SI" TO fin_suc1
    END-IF.
	IF (archALeer IS = 2)THEN
	    READ suc2 AT END MOVE "SI" TO fin_suc2
    END-IF.
	IF (archALeer IS = 3)THEN
  		READ suc3 AT END MOVE "SI" TO fin_suc3	
    END-IF.    
    IF (archALeer IS = 4)THEN
		READ tim  AT END MOVE "SI" TO fin_times
    END-IF.

5225_BLOQUEO.
	*> "XXXXX99999999XXXXXXX9999" es cuenta bloqueada  
    IF (fin_suc1 IS = "SI")THEN
	    MOVE "XXXXX99999999XXXXXXX9999" TO reg_suc1
    END-IF.
	
    IF (fin_suc2 IS = "SI")THEN
	    MOVE "XXXXX99999999XXXXXXX9999" TO reg_suc2
    END-IF.
	
    IF (fin_suc3 IS = "SI")THEN
	    MOVE "XXXXX99999999XXXXXXX9999" TO reg_suc3
    END-IF.
	
    IF (fin_times IS = "SI")THEN
	    MOVE "XXXXX99999999XXXXXXX9999" TO reg_time
    END-IF.

ARMAR_ENCABEZADO.
	IF (cont_titulo IS = 0)THEN
		MOVE fecha_hoy TO fechaHoy
		MOVE cant_hojas TO enc_num_hoja
		DISPLAY encabezado
		DISPLAY titulo
		MOVE prof_ant_min TO tit_num_prof
		MOVE prof_nom TO tit_nom_prof
		DISPLAY titulo_prof
		DISPLAY " "
		ADD 1 TO cant_hojas
		ADD 4 TO cont_titulo
	END-IF.

ARMAR_DISPLAY_FECHA_ANT.	
	DISPLAY culumnas_mat.
	DISPLAY linea.
	ADD 2 TO cont_titulo.

ARMAR_DISPLAY_FECHA.
	IF (cont_titulo IS = 0)THEN
		MOVE cant_horas_a_sumar TO reg1_horas
		MOVE sucursal_impr TO reg1_suc
		MOVE fecha_ant_min to reg1_fecha		
		MOVE v_tipo_clase(indi) TO aux_tipo_clase
		MOVE aux_tipo_clase TO reg1_tip_cla
		MOVE aux_tip_clase_tarifa TO reg1_tarifa
		MOVE aux_tip_clase_tarifa TO tarifa
		MULTIPLY cant_horas_a_sumar BY tarifa
		ADD tarifa TO TARIFA_FECHA
		ADD tarifa TO TARIFA_PROF
		ADD tarifa TO TARIFA_TOT
		MOVE tarifa TO reg1_impor
		ADD 1 TO cont_titulo
		DISPLAY reg1_con_fecha 
	ELSE
		MOVE cant_horas_a_sumar TO reg2_horas
		MOVE sucursal_impr TO reg2_suc 
		MOVE v_tipo_clase(indi) TO aux_tipo_clase
		MOVE aux_tipo_clase TO reg2_tip_cla
		MOVE aux_tip_clase_tarifa TO reg2_tarifa
		MOVE aux_tip_clase_tarifa TO tarifa
		MULTIPLY cant_horas_a_sumar BY tarifa
		ADD tarifa TO TARIFA_FECHA
		ADD tarifa TO TARIFA_PROF
		ADD tarifa TO TARIFA_TOT
		MOVE tarifa TO reg2_impor
		ADD 1 TO cont_titulo
		DISPLAY reg2_sin_fecha
	END-IF.

ARMAR_DISPLAY_FECHA_DESP.
	DISPLAY separador_tot.
	MOVE TOT_X_FECHA TO reg3_horas.
	MOVE TARIFA_FECHA TO reg3_impor.
	DISPLAY reg3_tot_fecha. 
	DISPLAY " ".
	ADD 3 TO cont_titulo.

ARMAR_DISPLAY_PROF.
	MOVE TOT_X_PROF TO reg4_horas.
	MOVE TARIFA_PROF TO reg4_impor
	DISPLAY reg4_tot_prof. 
	DISPLAY " ".
	ADD 2 TO cont_titulo.

ARMAR_DISPLAY_GRAL.
	MOVE TARIFA_TOT TO reg5_impor.
	DISPLAY reg5_tot_gral. 

OBTENER_INFO_PROF.
	READ profesores AT END MOVE "SI" TO fin_suc1.

INSERTAR_ANO.
    IF (ano IS = 2012)THEN
	    MOVE 1 TO anoi
    ELSE IF (ano IS = 2013)THEN
	    MOVE 2 TO anoi
    ELSE IF (ano IS = 2014)THEN
	    MOVE 3 TO anoi
    ELSE IF (ano IS = 2015)THEN
	    MOVE 4 TO anoi
    ELSE IF (ano IS = 2016)THEN
	    MOVE 5 TO anoi
    END-IF.

LLENAR_ANO.
    IF ( m IS = 1)THEN
	    MOVE "2012" TO ano_impr
    ELSE IF (m IS = 2)THEN
	    MOVE "2013" TO ano_impr
    ELSE IF (m IS = 3)THEN
	    MOVE "2014" TO ano_impr
    ELSE IF (m IS = 4)THEN
	    MOVE "2015" TO ano_impr
    ELSE IF (m IS = 5)THEN
	    MOVE "2016" TO ano_impr
    END-IF.
