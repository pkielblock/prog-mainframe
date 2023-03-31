       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG006.
       AUTHOR.       PEDRO.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-WRITTEN. 29/03/2023.
       DATE-COMPILED. 29/03/2023.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
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
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01) VALUE "N".
           05  WS-CTLIDO              PIC 9(02).
           05  WS-DATA                PIC 9(08).
           05  WS-AUXPCTACID          PIC 9(4)V99.
      *-----> DADOS DE ENTRADA
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(05).
           05 WS-ESTADO           PIC X(2).
           05 WS-QTD-VEICULOS     PIC 9(07).
           05 WS-BAFOMETRO        PIC X(01).
           05 WS-QTD-ACIDENTES    PIC 9(04).
           05 WS-QTD-OBITOS       PIC 9(04).
      *-----> DADOS DE SAIDA VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 CID                 PIC 99999.
           05 FILLER              PIC X(01) VALUE '-'.
           05 UF                  PIC XX.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 QTVEICS             PIC Z.ZZZ.ZZ9.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 BAFO                PIC X.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 QTACIDS             PIC Z.ZZ9.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 QTOBITOS            PIC Z.ZZ9.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 PACIDS              PIC ZZ9,99.
           05 FILLER              PIC X(01) VALUE '%'.

       LINKAGE SECTION.
      *----------------*
       01  LK-PARAMETROS.
           05 LK-NR-DPTO             PIC 9(04).
           05 LK-NOME-DPTO           PIC X(15).
           05 LK-COD-RETORNO         PIC 99.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL                                        *
      *--------------------------------------------------------------*

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 050-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS                                    *
      *--------------------------------------------------------------*
       010-INICIAR.

           ACCEPT WS-DATA FROM DATE
           DISPLAY "PEDRO"
           DISPLAY "ATIVIDADE 6"
           DISPLAY "ESTATISTICAS - DATA DO CALCULO:" WS-DATA (7:2) "/"
               WS-DATA (5:2) "/" "2" WS-DATA (2:3)
           MOVE  ZEROS  TO  WS-CTLIDO
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.
           ACCEPT WS-REG-SYSIN FROM SYSIN
           IF WS-REG-SYSIN = ALL '9'
              MOVE 'S' TO WS-FIM
           ELSE
              ADD 1 TO WS-CTLIDO
           END-IF
           COMPUTE WS-AUXPCTACID = WS-QTD-VEICULOS / WS-QTD-ACIDENTES
           MOVE WS-AUXPCTACID TO PACIDS
           MOVE WS-CIDADE TO CID
           MOVE WS-ESTADO TO UF
           MOVE WS-QTD-VEICULOS TO QTVEICS
           MOVE WS-BAFOMETRO TO BAFO
           MOVE WS-QTD-ACIDENTES TO QTACIDS
           MOVE WS-QTD-OBITOS TO QTOBITOS
           DISPLAY WS-REG-SYSOUT
           .
      *---------------> FIM DO PROGRAMA CGRPG006 <-------------------*
