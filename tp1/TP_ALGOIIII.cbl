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
		VALUE OF FILE-ID IS "/home/fernando/workspaces/workspace/algo4/algoIIII/tp1/archivos/tiposclase.txt".
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
	77 	sucursales-estado 				PIC XX. 
		88 sucursales-estado_ok 		VALUE "NO".
		88 sucursales-estado_eof 		VALUE "SI".
	77 	tipos_clase-estado 				PIC XX. 
		88 tipos_clase-estado_ok 		VALUE "NO".
		88 tipos_clase-estado_eof 		VALUE "SI".
	77 	mae-estado 						PIC XX.
		88 mae-estado_ok 				VALUE "NO". 
		88 mae-estado_eof 				VALUE "SI".
	77  WB-FIN-ENTRADA 					PIC X(1) VALUE "N".
    	88 FIN-ENTRADA 					VALUE "S".

   	*> TOTALES PARCIALES:
	01 TOT_GRAL 						PIC 9(6) VALUE 0.
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



PROCEDURE DIVISION.

*> CICLO PPAL--------------------------------------------------------------
	
	PERFORM 1_ABRO_ARCHIVOS.
	PERFORM 2_LEO_ARCHIVOS.
	PERFORM 3_ARMO_V_SUCURSALES.
	PERFORM 4_ARMO_V_TIPOS_CLASE.
	PERFORM 5_CICLO_ARCHIVOS UNTIL fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI".
	PERFORM 6_IMPRIMO_MATRIZ.
	PERFORM 7_CIERRO_ARCHIVOS.
	DISPLAY "TOT GRAL".
	MOVE TOT_GRAL TO TOT_IMPR.
	DISPLAY TOT_IMPR.
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


3_ARMO_V_SUCURSALES.
*> se trae a memoria el archivo de sucursales con su respectivo formato y de ser necesario su indice

4_ARMO_V_TIPOS_CLASE.
*> se trae a memoria el archivo de tipos_clase con su respectivo formato y de ser necesario su indice

5_CICLO_ARCHIVOS.
	PERFORM 51_OBTENER_REG_MIN_PROF.
	MOVE 0 TO TOT_X_PROF.	
	PERFORM 52_CICLO_PROFESORES UNTIL ((fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI") or prof_ant_min IS NOT = prof_min).
	PERFORM 53_ESCRIBO_TOT_PROF.
	PERFORM 54_IMPRIMO_TOT_PROF.
	MOVE prof_min TO prof_ant_min.
	*> corte control...	
    MOVE "CORTE POR PROFESOR      " to reg_mae.
    DISPLAY "CORTE POR PROFESOR".
	MOVE TOT_X_PROF TO TOT_IMPR.
	DISPLAY TOT_IMPR.
	WRITE reg_mae.
	*>...

6_IMPRIMO_MATRIZ.
*> se debera leer toda la matriz (punto b) y mostrarla en el formato del enunciado 

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
	PERFORM 521_OBTENER_REG_MIN.
	MOVE 0 TO TOT_X_FECHA.
	PERFORM 522_CICLO_FECHA UNTIL ((fin_suc1 IS = "SI" and fin_suc2 IS = "SI" and fin_suc3 IS = "SI" and fin_times IS = "SI") or prof_min IS NOT = prof_ant_min or fecha_min IS NOT = fecha_ant_min).
*> corte cuando eof de todos los archivos: EOF suc1 and EOF suc2 and EOF suc3 and EOF times and Prof_ant != Prof_act and Fecha_ant != Fecha_act
	PERFORM 523_ESCRIBO_TOT_FECHA.

	IF (fecha_ant_min IS NOT = fecha_min ) THEN 	
		MOVE "CORTE POR FECHA         " to reg_mae
		WRITE reg_mae
		DISPLAY "CORTE POR FECHA"
		MOVE TOT_X_FECHA TO TOT_IMPR
		DISPLAY TOT_IMPR
	END-IF.

	MOVE fecha_min TO fecha_ant_min.

53_ESCRIBO_TOT_PROF.
*> se debe escribir en el archivo master, el total por profesor como indica el enunciado

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

523_ESCRIBO_TOT_FECHA.
*> se debe escribir en el archivo master, el total por fecha como indica el enunciado


*> CICLO IND--------------------------------------------------------------

5221_SUMAR_TOTALES.
*> se debe sumar en todos los totales: 	01 TOT_GRAL, TOT_POR_PROF y TOT_POR_FECHA el movimiento individual.
    IF (archALeer IS = 1 and fin_suc1 IS NOT = "SI")THEN
		ADD suc1_horas TO TOT_GRAL
		ADD suc1_horas TO TOT_X_PROF
		ADD suc1_horas TO TOT_X_FECHA
		ADD suc1_horas TO TOT_X_SUC
    END-IF.
	
    IF (archALeer IS = 2 and fin_suc2 IS NOT = "SI")THEN
		ADD suc2_horas TO TOT_GRAL
		ADD suc2_horas TO TOT_X_PROF
		ADD suc2_horas TO TOT_X_FECHA
		ADD suc2_horas TO TOT_X_SUC
    END-IF.
	
    IF (archALeer IS = 3 and fin_suc3 IS NOT = "SI")THEN
		ADD suc3_horas TO TOT_GRAL
		ADD suc3_horas TO TOT_X_PROF
		ADD suc3_horas TO TOT_X_FECHA
		ADD suc3_horas TO TOT_X_SUC
    END-IF.
	
    IF (archALeer IS = 4 and fin_times IS NOT = "SI")THEN
		ADD tim_horas TO TOT_GRAL
		ADD tim_horas TO TOT_X_PROF
		ADD tim_horas TO TOT_X_FECHA
		ADD tim_horas TO TOT_X_SUC
    END-IF.


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
