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
	SELECT TIPOSCLASE ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS tipos_clase-estado.
    SELECT MASTER ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS mae-estado.
DATA DIVISION.
FILE SECTION.

FD SUC1 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/suc1.txt".
01 reg_suc1.
	  03 suc1_num 						PIC X(5).
	  03 suc1_fecha 					PIC 9(8).
	  03 suc1_suc 						PIC X(3).
	  03 suc1_clase 					PIC X(4).
	  03 suc1_horas 					PIC 9(2)V99.
	  
FD SUC2 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/suc2.txt".
01 reg_suc2.
	  03 suc2_num 						PIC X(5).
	  03 suc2_fecha 					PIC 9(8).
	  03 suc2_suc 						PIC X(3).
	  03 suc2_clase 					PIC X(4).
	  03 suc2_horas 					PIC 9(2)V99.
	  
FD SUC3 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/suc3.txt".
01 reg_suc3.
	  03 suc3_num 						PIC X(5).
	  03 suc3_fecha 					PIC 9(8).
	  03 suc3_suc 						PIC X(3).
	  03 suc3_clase 					PIC X(4).
	  03 suc3_horas 					PIC 9(2)V99.
	  
FD TIM LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/times.txt".
01 reg_time.
	  03 tim_num 						PIC X(5).
	  03 tim_fecha 						PIC 9(8).
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

   	*> TOTALES PARCIALES:
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

 	01 cont_titulo 						PIC 9(2) VALUE 0.

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
	01 fecha_hoy 						PIC X(10) VALUE "12/12/1234".

	01 linea_impr 						PIC X(100).
	01 nom_prof  						PIC X(25) VALUE " ". 
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

	01 titulo_prof. 						
	  03 prof 							PIC X(9) VALUE "Profesor:".
	  03 nada 							PIC X(1) VALUE " ". 
	  03 tit_num_prof					PIC X(5) VALUE "XXXXX".
	  03 nada 							PIC X(10) VALUE ALL " ".
	  03 nom_prof 						PIC X(7) VALUE "Nombre:".
	  03 tit_nom_prof					PIC X(25) VALUE ALL "X".

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
	  03 reg1_impor						PIC X(10) VALUE "ZZZZZZ9,99".


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
	  03 reg2_impor						PIC X(10) VALUE "ZZZZZZ9,99".

	01 separador_tot.
	  03 nada 							PIC X(60) VALUE ALL " ".
	  03 sep 							PIC X(5) VALUE ALL "-".
	  03 nada 							PIC X(3) VALUE ALL " ".
	  03 sep 							PIC X(10) VALUE ALL "-".
	
	01 reg3_tot_fecha. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg3_tit 						PIC X(17) VALUE "Totales por fecha".
	  03 nada 							PIC X(40) VALUE ALL " ".
	  03 reg3_horas 					PIC 9(3)V99.
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg3_impor						PIC X(11) VALUE "ZZZZZZZ9,99".

	01 reg4_tot_prof. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg4_tit 						PIC X(20) VALUE "Totales por profesor".
	  03 nada 							PIC X(36) VALUE ALL " ".
	  03 reg4_horas 					PIC 9(4)V99.
	  03 nada 							PIC X(1) VALUE ALL " ".
	  03 reg4_impor						PIC X(12) VALUE "ZZZZZZZZ9,99".

	01 reg5_tot_gral. 
	  03 nada 							PIC X(2) VALUE ALL " ".
	  03 reg5_tit 						PIC X(13) VALUE "Total general".
	  03 nada 							PIC X(50) VALUE ALL " ".
	  03 reg5_impor						PIC X(13) VALUE "ZZZZZZZZZ9,99".
	
PROCEDURE DIVISION.

*> CICLO PPAL--------------------------------------------------------------
	PERFORM ARMAR_ENCABEZADO.
	PERFORM 1_ABRO_ARCHIVOS.
	PERFORM 2_LEO_ARCHIVOS.
	PERFORM 3_CICLO_SUCURSALES UNTIL sucursales-estado_eof.
	PERFORM 4_CICLO_TIPOS_CLASE UNTIL tipos_clase-estado_eof.
	PERFORM 5_CICLO_ARCHIVOS UNTIL fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI".
	PERFORM 6_IMPRIMO_MATRIZ.
	PERFORM 7_CIERRO_ARCHIVOS.
	*>DISPLAY "TOT GRAL".
	PERFORM ARMAR_DISPLAY_GRAL.
	MOVE TOT_GRAL TO TOT_IMPR.
	*>DISPLAY TOT_IMPR.
	STOP RUN.   


*> --------------------------------------------------------------

1_ABRO_ARCHIVOS.
	OPEN INPUT SUC1.
	OPEN INPUT SUC2.
	OPEN INPUT SUC3.
	OPEN INPUT TIM.
	OPEN INPUT SUCURSALES.
	OPEN INPUT TIPOSCLASE.
	OPEN OUTPUT MASTER.

2_LEO_ARCHIVOS.
	READ suc1 AT END MOVE "SI" TO fin_suc1.
	READ suc2 AT END MOVE "SI" TO fin_suc2.
	READ suc3 AT END MOVE "SI" TO fin_suc3.
	READ TIM  AT END MOVE "SI" TO fin_times.

	*> inicializo prof_ant sabiendo que hay un registro q la va a llenar 
	MOVE "NNNNN" TO prof_ant_min.
	MOVE "NNNNNNNN" TO fecha_ant_min.
	*> caso 1º, ventaja que nadie la pudo inicializar antes
	IF (fin_suc1 IS = "NO")THEN
		MOVE suc1_num TO prof_ant_min
		MOVE suc1_fecha TO fecha_ant_min
	END-IF.
	*> caso 2º, completo
	IF (fin_suc2 IS = "NO" AND prof_ant_min IS = "NNNNN")THEN
		MOVE suc2_num TO prof_ant_min
		MOVE suc2_fecha TO fecha_ant_min		
	ELSE IF (fin_suc2 IS = "NO" AND suc2_num IS < prof_ant_min) THEN 	
		MOVE suc2_num TO prof_ant_min
		MOVE suc2_fecha TO fecha_ant_min
	ELSE IF (fin_suc2 IS = "NO" AND suc2_num IS = prof_ant_min AND suc2_fecha IS < fecha_ant_min) THEN 	
		MOVE suc2_num TO prof_ant_min
		MOVE suc2_fecha TO fecha_ant_min
	END-IF.
	*> caso 3º, completo
	IF (fin_suc3 IS = "NO" AND prof_ant_min IS = "NNNNN")THEN
		MOVE suc3_num TO prof_ant_min
		MOVE suc3_fecha TO fecha_ant_min
	ELSE IF (fin_suc3 IS = "NO" AND suc3_num IS < prof_ant_min) THEN 	
		MOVE suc3_num TO prof_ant_min
		MOVE suc3_fecha TO fecha_ant_min
	ELSE IF (fin_suc3 IS = "NO" AND suc3_num IS = prof_ant_min AND suc3_fecha IS < fecha_ant_min) THEN 	
		MOVE suc3_num TO prof_ant_min
		MOVE suc3_fecha TO fecha_ant_min
	END-IF.
    *> caso 4º, completo
	IF (fin_times IS = "NO" AND prof_ant_min IS = "NNNNN")THEN
		MOVE tim_num TO prof_ant_min
		MOVE tim_fecha TO fecha_ant_min
	ELSE IF (fin_times IS = "NO" AND tim_num IS < prof_ant_min) THEN 	
		MOVE tim_num TO prof_ant_min
		MOVE tim_fecha TO fecha_ant_min
	ELSE IF (fin_times IS = "NO" AND tim_num IS = prof_ant_min AND tim_fecha IS < fecha_ant_min) THEN 	
		MOVE tim_num TO prof_ant_min
		MOVE tim_fecha TO fecha_ant_min
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
	MOVE 0 TO TOT_X_PROF.	
	PERFORM 52_CICLO_PROFESORES UNTIL 
	((fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI") 
	or (prof_ant_min IS NOT = suc1_num and prof_ant_min IS NOT = suc2_num and prof_ant_min IS NOT = suc3_num and prof_ant_min IS NOT = tim_num)).
	
		*> corte control...	
	    *>MOVE "CORTE POR PROFESOR      " to reg_mae.
	    *>WRITE reg_mae.
	    *>DISPLAY "CORTE POR PROFESOR".
		*>DISPLAY "---------------".
		*>DISPLAY prof_ant_min.
		*>DISPLAY prof_min.
		*>MOVE TOT_X_PROF TO TOT_IMPR.
		*>DISPLAY TOT_IMPR.
		*>DISPLAY "---------------".
		*>...
	PERFORM 53_ESCRIBO_TOT_PROF.
	PERFORM 54_IMPRIMO_TOT_PROF.
	MOVE prof_min TO prof_ant_min.

6_IMPRIMO_MATRIZ.
*> se debera leer toda la matriz (punto b) y mostrarla en el formato del enunciado 

*>	MOVE 1 TO I.
*>	PERFORM HOLA UNTIL I > 50.

*>HOLA.
*>	MOVE I TO indi.
	*>DISPLAY v_tipo_clase(indi).
*>	ADD 1 TO I.

7_CIERRO_ARCHIVOS.
	CLOSE SUC1.
	CLOSE SUC2.
	CLOSE SUC3.
	CLOSE TIM.
	CLOSE SUCURSALES.
	CLOSE TIPOSCLASE.
	CLOSE MASTER.

*> CICLO POR PROFESOR--------------------------------------------------------------

51_OBTENER_REG_MIN_PROF.
	*> inicializo prof_min sabiendo que hay un registro q la va a llenar 
	MOVE "NNNNN" TO prof_min.
	*> caso 1º, ventaja que nadie la pudo inicializar antes
	IF (fin_suc1 IS not = "SI")THEN
		MOVE suc1_num TO prof_min
	END-IF.
	*> caso 2º, completo
	IF (fin_suc2 IS not = "SI" AND prof_min IS = "NNNNN")THEN
		MOVE suc2_num TO prof_min
	ELSE IF (fin_suc2 IS not = "SI" AND suc2_num IS < prof_min) THEN 	
		MOVE suc2_num TO prof_min
	END-IF.
	*> caso 3º, completo
	IF (fin_suc3 IS not = "SI" AND prof_min IS = "NNNNN")THEN
		MOVE suc3_num TO prof_min
	ELSE IF (fin_suc3 IS not = "SI" AND suc3_num IS < prof_min) THEN 	
		MOVE suc3_num TO prof_min
	END-IF.
    *> caso 4º, completo
	IF (fin_times IS not = "SI" AND prof_min IS = "NNNNN")THEN
		MOVE tim_num TO prof_min
	ELSE IF (fin_times IS not = "SI" AND tim_num IS < prof_min) THEN 	
		MOVE tim_num TO prof_min
	END-IF.

52_CICLO_PROFESORES.
	PERFORM ARMAR_DISPLAY_FECHA_ANT.
	PERFORM 521_OBTENER_REG_MIN.
	MOVE 0 TO TOT_X_FECHA.
	PERFORM 522_CICLO_FECHA UNTIL 
	((fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI") 
	or (prof_ant_min IS NOT = suc1_num and prof_ant_min IS NOT = suc2_num and prof_ant_min IS NOT = suc3_num and prof_ant_min IS NOT = tim_num)
	or (fecha_ant_min IS NOT = suc1_fecha and fecha_ant_min IS NOT = suc2_fecha and fecha_ant_min IS NOT = suc3_fecha and fecha_ant_min IS NOT = tim_fecha)).
*> corte cuando eof de todos los archivos: EOF suc1 and EOF suc2 and EOF suc3 and EOF times and Prof_ant != Prof_act and Fecha_ant != Fecha_act
	
	PERFORM 523_ESCRIBO_TOT_FECHA.

	IF (fecha_ant_min IS NOT = fecha_min ) THEN 	
		MOVE "CORTE POR FECHA         " to reg_mae
		WRITE reg_mae
		*>DISPLAY "CORTE POR FECHA"
		*>DISPLAY "---------------"
		*>DISPLAY fecha_ant_min
		*>DISPLAY fecha_min
		MOVE TOT_X_FECHA TO TOT_IMPR
		*>DISPLAY TOT_IMPR
		*>DISPLAY "---------------"

	END-IF.

	MOVE fecha_min TO fecha_ant_min.

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
	PERFORM 521_OBTENER_REG_MIN.
	PERFORM 5221_SUMAR_TOTALES.
	PERFORM 5222_SUMAR_EN_MATRIZ.
	PERFORM 5223_ESCRIBO_MOV.
	PERFORM 5224_LEO_ARCH_MIN.
	PERFORM 5225_BLOQUEO.

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
    END-IF.
	
    IF (archALeer IS = 2 and fin_suc2 IS NOT = "SI")THEN
		MOVE suc2_horas TO cant_horas_a_sumar
	    MOVE suc2_suc TO sucursal_impr

	    MOVE suc2_suc TO ind
	    MOVE suc2_clase TO indi
    END-IF.
	
    IF (archALeer IS = 3 and fin_suc3 IS NOT = "SI")THEN
   		MOVE suc3_horas TO cant_horas_a_sumar
   		MOVE suc3_suc TO sucursal_impr

	    MOVE suc3_suc TO ind
	    MOVE suc3_clase TO indi
    END-IF.
	
    IF (archALeer IS = 4 and fin_times IS NOT = "SI")THEN
		MOVE tim_horas TO cant_horas_a_sumar
		MOVE tim_suc TO sucursal_impr

	    MOVE tim_suc TO ind
	    MOVE tim_clase TO indi
    END-IF.

	ADD cant_horas_a_sumar TO TOT_GRAL.
	ADD cant_horas_a_sumar TO TOT_X_PROF.
	ADD cant_horas_a_sumar TO TOT_X_FECHA.
	ADD cant_horas_a_sumar TO TOT_X_SUC.

    PERFORM ARMAR_DISPLAY_FECHA.

5222_SUMAR_EN_MATRIZ.
*> se debe sumar en la matriz para el punto b de a cuerdo a la sucursal, año y mes

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

*>ARMAR_PAG.
*>	PERFORM ARMAR_ENCABEZADO.
*>	PERFORM ARMAR_DISPLAY_FECHA.
*>	PERFORM ARMAR_DISPLAY_FECHA.
*>	PERFORM ARMAR_DISPLAY_PROF.
*>	PERFORM ARMAR_DISPLAY_GRAL.

*> falta
ARMAR_ENCABEZADO.
	IF (cont_titulo IS = 0)THEN
		MOVE fecha_hoy TO fechaHoy
		MOVE cant_hojas TO enc_num_hoja
		DISPLAY encabezado
		DISPLAY titulo
		MOVE prof_min TO tit_num_prof
		*>MOVE nom_prof_min TO nom_prof.
		DISPLAY titulo_prof
		DISPLAY " "
	END-IF.

*> listo
ARMAR_DISPLAY_FECHA_ANT.	
	DISPLAY culumnas_mat.
	DISPLAY linea.

*> falta
ARMAR_DISPLAY_FECHA.
	IF (cont_titulo IS = 0)THEN
		MOVE cant_horas_a_sumar TO reg1_horas
		MOVE sucursal_impr TO reg1_suc
		MOVE fecha_min to reg1_fecha		
		MOVE v_tipo_clase(indi) TO aux_tipo_clase
		MOVE aux_tipo_clase TO reg1_tip_cla
		MOVE aux_tip_clase_tarifa TO reg1_tarifa
		DISPLAY reg1_con_fecha 
	ELSE
		MOVE cant_horas_a_sumar TO reg2_horas
		MOVE sucursal_impr TO reg2_suc 
		MOVE v_tipo_clase(indi) TO aux_tipo_clase
		MOVE aux_tipo_clase TO reg2_tip_cla
		MOVE aux_tip_clase_tarifa TO reg1_tarifa
		DISPLAY reg2_sin_fecha
	END-IF.

*> falta	
ARMAR_DISPLAY_FECHA_DESP.
	DISPLAY separador_tot.

	MOVE TOT_X_FECHA TO reg3_horas.
	*>MOVE tot_impor TO reg3_impor.
	DISPLAY reg3_tot_fecha. 
	DISPLAY " ".

*> falta
ARMAR_DISPLAY_PROF.
	MOVE TOT_X_PROF TO reg4_horas.
	*>MOVE  ... TO reg4_impor
	DISPLAY reg4_tot_prof. 
	DISPLAY " ".

*> listo	
ARMAR_DISPLAY_GRAL.
	MOVE TOT_GRAL TO reg5_impor.
	DISPLAY reg5_tot_gral. 
