       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CSPRG004.
       AUTHOR. CELSO GALLAO.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-WRITTEN. 20-FEV-2023.
       DATE-COMPILED. 22-MAR-2023.
       SECURITY. NIVEL BASICO.
      *---------------------*
       ENVIRONMENT DIVISION.
      *======================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       FILE-CONTROL.
      *==> LOCAL PARA O SELECT DOS ARQUVOS

       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
      *==> LOCAL PARA A FD (DESCRICAO DOS ARQUIVOS)

       WORKING-STORAGE SECTION.
      *-----------------------*

      *-----> AREA AUXILIAR
       77  WS-FIM                 PIC X(01) VALUE 'N'.
       77  WS-CTEXIB              PIC 9(02).
       77  AS-CEP                 PIC 9(08).
       77  AS-FRENTE              PIC 9(03)V99.
       77  AS-COMP                PIC 9(03)V99.
       77  AS-VAL-M2              PIC 9(05)V99.
       77  AS-VAL-VENDA           PIC 9(08)V99.
       77  AS-COMISSAO            PIC 9(07)V99.
       77  AS-DATA                PIC 9(08).
       77  AS-HORA                PIC 99.99.9999.

      *-----> DADOS DE SAIDA VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-CEP              PIC 9(08).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-FRENTE           PIC ZZ9,99.
           05 FILLER              PIC X(02) VALUE 'M '.
           05 WS-COMP             PIC ZZ9,99.
           05 FILLER              PIC X(02) VALUE 'M '.
           05 WS-VAL-M2           PIC $$.$$9,99.
           05 FILLER              PIC X(03) VALUE 'M2 '.
           05 WS-VAL-VENDA        PIC $$.$$$.$$9,99.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-COMISSAO         PIC $.$$$.$$9,99.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-MENSAGEM         PIC X(12).

       LINKAGE SECTION.
      *----------------*
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL                                        *
      *--------------------------------------------------------------*
           ACCEPT AS-DATA FROM DATE
           ACCEPT AS-HORA FROM TIME

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 050-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS                                    *
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "ATIVIDADE 4"

           DISPLAY 'PEDRO'
           DISPLAY 'CALCULO DO PRECO DE VENDA DE UM TERRENO RETANGULAR'
           DISPLAY 'DATA DO CALCULO: ' AS-DATA (7:2) "/"
               AS-DATA (5:2) "/" "2" AS-DATA (2:3)
           DISPLAY 'HORA DO CALCULO: ' AS-HORA
           DISPLAY '---------------------------------------------------'
           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.

           MOVE  09000300           TO   AS-CEP
           MOVE  12,35              TO   AS-FRENTE
           MOVE  62,00              TO   AS-COMP
           MOVE  2315,00            TO   AS-VAL-M2
           COMPUTE AS-VAL-VENDA = (AS-FRENTE * AS-COMP * AS-VAL-M2)
           IF AS-VAL-VENDA > 1500000,00
               COMPUTE AS-COMISSAO = AS-VAL-VENDA * 0,04
               MOVE 'ALTO PADRAO' TO WS-MENSAGEM
           ELSE
               COMPUTE AS-COMISSAO = AS-VAL-VENDA * 0,06
               MOVE 'MEDIO PADRAO' TO WS-MENSAGEM
           END-IF

           MOVE AS-CEP             TO WS-CEP
           MOVE AS-FRENTE          TO WS-FRENTE
           MOVE AS-COMP            TO WS-COMP
           MOVE AS-VAL-M2          TO WS-VAL-M2
           MOVE AS-VAL-VENDA       TO WS-VAL-VENDA
           MOVE AS-COMISSAO        TO WS-COMISSAO

           DISPLAY WS-REG-SYSOUT
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '---------------------------------------------------'
           DISPLAY '** ENCERRANDO A EXECUCAO **'
           DISPLAY "REGISTROS EXIBIDOS = " WS-CTEXIB
           DISPLAY "TERMINO NORMAL DO PROGRAMA CGPRG004"
           .
      *---------------> FIM DO PROGRAMA CGPRG004 <-------------------*
