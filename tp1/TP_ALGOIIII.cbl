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
	SELECT  TIPOSCLASE ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS tipos_clase-estado.
    SELECT MASTER ASSIGN TO maestro STATUS IS mae-estado.
DATA DIVISION.
FILE SECTION.

FD SUC1 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/Escritorio/suc1.txt".
01 reg_suc1 PIC X(1).

FD SUC2 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/Escritorio/suc2.txt".
01 reg_suc2 PIC X(1).

FD SUC3 LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/Escritorio/suc3.txt".
01 reg_suc3 PIC X(1).

FD TIM LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/Escritorio/times.txt".
01 reg_times PIC X(1).

FD SUCURSALES LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/Escritorio/sucursales.txt".
01 reg_sucursal PIC X(1).

FD TIPOSCLASE LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/Escritorio/tiposClase.txt".
01 reg_tipo_clase PIC X(1).

FD MASTER LABEL RECORD IS STANDARD 
		VALUE OF FILE-ID IS "/home/fernando/Escritorio/mae.txt".
01 reg_mae PIC X(1).

WORKING-STORAGE SECTION.

	77 	suc1_estado 					PIC XX. 
		88 suc1_estado_ok 				VALUE "NO".
		88 suc1_estado_eof 				VALUE "SI".
	77 	suc2_estado 					PIC XX. 
		88 suc2_estado_ok 				VALUE "NO".
		88 suc2_estado_eof 				VALUE "SI".
	77 	suc3_estado 					PIC XX. 
		88 suc3_estado_ok 				VALUE "NO".
		88 suc3_estado_eof 				VALUE "SI".
	77 	times_estado 					PIC XX. 
		88 times_estado_ok 				VALUE "NO".
		88 times_estado_eof				VALUE "SI".
	77 	sucursales_estado 				PIC XX. 
		88 sucursales_estado_ok 		VALUE "NO".
		88 sucursales_estado_eof 		VALUE "SI".
	77 	tipos_clase 					PIC XX. 
		88 tipos_clase_estado_ok 		VALUE "NO".
		88 tipos_clase_estado_eof 		VALUE "SI".
	77 	mae_estado 						PIC XX.
		88 mae_estado_ok 				VALUE "NO". 
		88 mae_estado_eof 				VALUE "SI".
	77 WB-FIN-ENTRADA 					PIC X(1) VALUE "N".
    	88 FIN-ENTRADA 					VALUE "S".

	01 TOT_GRAL 						PIC 9(6) VALUE 0.
	01 TOT_X_PROF 						PIC 9(6) VALUE 0.
	01 TOT_X_FECHA 						PIC 9(6) VALUE 0.
	01 I 								PIC 9(1) VALUE 0.
	01 suc1-estado 						PIC XX.
	01 suc2-estado 						PIC XX.
	01 suc3-estado 						PIC XX.
	01 times-estado 					PIC XX.
	01 sucursales-estado 				PIC XX.
	01 tipos_clase-estado 				PIC XX.
	01 mae-estado						PIC XX.
PROCEDURE DIVISION.

*> CICLO PPAL--------------------------------------------------------------

	PERFORM 1_ABRO_ARCHIVOS.
	PERFORM 2_LEO_ARCHIVOS.
	PERFORM 3_ARMO_V_SUCURSALES.
	PERFORM 4_ARMO_V_TIPOS_CLASE.
	PERFORM 5_CICLO_ARCHIVOS UNTIL I > 2.
*> corte cuando eof de todos los archivos: EOF suc1 and EOF suc2 and EOF suc3 and EOF times
	PERFORM 6_IMPRIMO_MATRIZ.
	PERFORM 7_CIERRO_ARCHIVOS.
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
	READ SUC1 AT END MOVE "SI" TO suc1-estado.

*> primera lectura de los archivos suc1, suc2, suc3 y times y guardado en un registo particular de cada uno ej: reg_suc1 = Leo(suc1)

3_ARMO_V_SUCURSALES.
*> se trae a memoria el archivo de sucursales con su respectivo formato y de ser necesario su indice

4_ARMO_V_TIPOS_CLASE.
*> se trae a memoria el archivo de tipos_clase con su respectivo formato y de ser necesario su indice

5_CICLO_ARCHIVOS.
	PERFORM 51_OBTENER_REG_MIN_PROF.
	MOVE 0 TO TOT_X_PROF.
	PERFORM 52_CICLO_PROFESORES UNTIL I > 2.
*> corte cuando eof de todos los archivos: EOF suc1 and EOF suc2 and EOF suc3 and EOF times and Prof_ant != Prof_act
	PERFORM 53_ESCRIBO_TOT_PROF.
	PERFORM 54_IMPRIMO_TOT_PROF.

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
*> se debe: inicializar el reg_min con el minimo de entre: reg_suc1, reg_suc2, reg_suc3, reg_times (comparando por numero de profesor) ya que se debe inicializar prof_anterior

52_CICLO_PROFESORES.
	PERFORM 521_OBTENER_REG_MIN.
	MOVE 0 TO TOT_X_FECHA.
	PERFORM 522_CICLO_FECHA UNTIL I > 2.
*> corte cuando eof de todos los archivos: EOF suc1 and EOF suc2 and EOF suc3 and EOF times and Prof_ant != Prof_act and Fecha_ant != Fecha_act
	PERFORM 523_ESCRIBO_TOT_FECHA.

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

522_CICLO_FECHA.
	PERFORM 5221_SUMAR_TOTALES.
	PERFORM 5222_SUMAR_EN_MATRIZ.
	PERFORM 5223_ESCRIBO_MOV.
	PERFORM 5224_LEO_ARCH_MIN.
	
523_ESCRIBO_TOT_FECHA.
*> se debe escribir en el archivo master, el total por fecha como indica el enunciado


*> CICLO IND--------------------------------------------------------------

5221_SUMAR_TOTALES.
*> se debe sumar en todos los totales: 	01 TOT_GRAL, TOT_POR_PROF y TOT_POR_FECHA el movimiento individual.

5222_SUMAR_EN_MATRIZ.
	ADD 1 TO I.
*> se debe sumar en la matriz para el punto b de a cuerdo a la sucursal, año y mes

5223_ESCRIBO_MOV.
	DISPLAY reg_suc1.
*> se debe escribir el movimiento individual como lo dice el enunciado

5224_LEO_ARCH_MIN.
	READ suc1 AT END MOVE "SI" TO suc1_estado.
*>  ya teniendo el archivo min, se debe dejar el resultado en el nuevo registro.
