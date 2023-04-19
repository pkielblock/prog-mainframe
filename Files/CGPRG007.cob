       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG007.
       AUTHOR.       PEDRO KIELBLOCK.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-WRITTEN. 20/10/2020.
       DATE-COMPILED. 19/04/2023.
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
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-RGLIDO              PIC 9(02).
           05  WS-DATA                PIC 9999/99/99.
           05  WS-PORC                PIC 99V9999.
           05  WS-SOMA-SP             PIC 99V9999.
           05  WS-CONT-SP             PIC 9(02).
           05  WS-MEDIA-SP            PIC 99V9999.
           05  MEDIA-SP               PIC ZZ9,99.
           05  WS-QTD-ASP             PIC 9(05).
           05  WS-QTD-CSP             PIC 99.
           05  QTD-ACSP               PIC ZZ.ZZ9.
           05  WS-CID-MAIOR           PIC 9(05).
           05  WS-QTD-MAIOR-CID       PIC 9(04).
           05  QTD-MAIOR-CID          PIC Z.ZZ9.
           05  WS-CID-MENOR           PIC 9(05).
           05  WS-M-PORC              PIC 99V9999.
           05  M-PORC                 PIC ZZ9,99.
           05  WS-QTD-M-OBITO         PIC 9(04).
           05  WS-QTD-M-ACID          PIC 9(05).
           05  AS-HORA                PIC 99.99.9999.
           05  WS-SOMA-RJ             PIC 99V9999.
           05  WS-CONT-RJ             PIC 9(02).
           05  WS-MEDIA-RJ            PIC 99V9999.
           05  MEDIA-RJ               PIC ZZ9,99.
           05  WS-QTD-ARJ             PIC 9(05).
           05  WS-QTD-CRJ             PIC 99.
           05  QTD-ACRJ               PIC ZZ.ZZ9.
           05  WS-SOMA-MG             PIC 99V9999.
           05  WS-CONT-MG             PIC 9(02).
           05  WS-MEDIA-MG            PIC 99V9999.
           05  MEDIA-MG               PIC ZZ9,99.
           05  WS-QTD-AMG             PIC 9(05).
           05  WS-QTD-CMG             PIC 99.
           05  QTD-ACMG               PIC ZZ.ZZ9.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(05).
           05 WS-ESTADO           PIC X(2).
           05 WS-QTD-VEICULOS     PIC 9(07).
           05 WS-BAFOMETRO        PIC X(01).
           05 WS-QTD-ACIDENTES    PIC 9(04).
           05 WS-QTD-OBITOS       PIC 9(04).

      *----> SAIDA DE DADOS - VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 CID                  PIC 99999.
           05 FILLER               PIC X(01)       VALUE "-".
           05 UF                   PIC XX.
           05 FILLER               PIC X(04)       VALUE SPACES.
           05 QTVEICS              PIC Z.ZZZ.ZZ9.
           05 FILLER               PIC X(04)       VALUE SPACES.
           05 BAFO                 PIC X.
           05 FILLER               PIC X(04)       VALUE SPACES.
           05 QTACIDS              PIC Z.ZZ9.
           05 FILLER               PIC X(04)       VALUE SPACES.
           05 QTOBITOS             PIC Z.ZZ9.
           05 FILLER               PIC X(04)       VALUE SPACES.
           05 PACIDS               PIC Z9,99.
           05 FILLER               PIC X(01)       VALUE "%".
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG006.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 040-PROCESSAR-SP
           PERFORM 041-PROCESSAR-RJ
           PERFORM 042-PROCESSAR-MG
           PERFORM 045-PROCESSAR-MAIOR
           PERFORM 047-PROCESSAR-MENOR
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.
           ACCEPT  WS-DATA FROM DATE
           ACCEPT  AS-HORA FROM TIME
           DISPLAY "PEDRO KIELBLOCK"
           DISPLAY "ATIVIDADE 7"
           DISPLAY "ESTATISTICAS - DATA: " WS-DATA(9:2) "/"
                      WS-DATA(6:2) "/2" WS-DATA(2:3) " - HORA: "
                      AS-HORA
           DISPLAY "-------------------------------------------"
           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-RGLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

           COMPUTE WS-PORC = (WS-QTD-ACIDENTES / WS-QTD-VEICULOS) * 100
           MOVE WS-PORC            TO  PACIDS
           MOVE WS-CIDADE          TO  CID
           MOVE WS-ESTADO          TO  UF
           MOVE WS-QTD-VEICULOS    TO  QTVEICS
           MOVE WS-BAFOMETRO       TO  BAFO
           MOVE WS-QTD-ACIDENTES   TO  QTACIDS
           MOVE WS-QTD-OBITOS      TO  QTOBITOS
           IF WS-ESTADO = 'SP'
              ADD WS-QTD-ACIDENTES TO WS-QTD-ASP
              ADD 1       TO WS-QTD-CSP
              ADD WS-PORC TO WS-SOMA-SP
              ADD 1       TO WS-CONT-SP
           END-IF
           IF WS-ESTADO = 'RJ'
              ADD WS-QTD-ACIDENTES TO WS-QTD-ARJ
              ADD 1       TO WS-QTD-CRJ
              ADD WS-PORC TO WS-SOMA-RJ
              ADD 1       TO WS-CONT-RJ
           END-IF
           IF WS-ESTADO = 'MG'
              ADD WS-QTD-ACIDENTES TO WS-QTD-AMG
              ADD 1       TO WS-QTD-CMG
              ADD WS-PORC TO WS-SOMA-MG
              ADD 1       TO WS-CONT-MG
           END-IF
           IF WS-RGLIDO = 1
              MOVE WS-QTD-ACIDENTES TO WS-QTD-MAIOR-CID
              MOVE WS-CIDADE        TO WS-CID-MAIOR
              MOVE WS-CIDADE        TO WS-CID-MENOR
              MOVE WS-PORC          TO WS-M-PORC
           END-IF
           IF WS-QTD-ACIDENTES > WS-QTD-MAIOR-CID
              MOVE WS-QTD-ACIDENTES TO WS-QTD-MAIOR-CID
              MOVE WS-CIDADE        TO WS-CID-MAIOR
           END-IF
           IF WS-PORC < WS-M-PORC
              MOVE WS-CIDADE        TO WS-CID-MENOR
              MOVE WS-PORC          TO WS-M-PORC
              MOVE WS-QTD-OBITOS    TO WS-QTD-M-OBITO
              MOVE WS-QTD-ACIDENTES TO WS-QTD-M-ACID
           END-IF
           DISPLAY WS-REG-SYSOUT
           PERFORM 025-LER-SYSIN
           .
       040-PROCESSAR-SP.

            COMPUTE WS-MEDIA-SP = WS-SOMA-SP / WS-CONT-SP
            MOVE WS-MEDIA-SP TO MEDIA-SP
            MOVE WS-QTD-ASP  TO QTD-ACSP
            DISPLAY '---------------------------------'
            DISPLAY 'MEDIA DAS PROCENTAGENS DE SP.....: ' MEDIA-SP "%"
            DISPLAY 'QTD. DE ACIDENTES TOTAIS DE SP...: ' QTD-ACSP
            DISPLAY 'QTD. DE CIDADES DE SP PESQUISADAS: ' WS-QTD-CSP
            .
       041-PROCESSAR-RJ.

            COMPUTE WS-MEDIA-RJ = WS-SOMA-RJ / WS-CONT-RJ
            MOVE WS-MEDIA-RJ TO MEDIA-RJ
            MOVE WS-QTD-ARJ  TO QTD-ACRJ
            DISPLAY '---------------------------------'
            DISPLAY 'MEDIA DAS PROCENTAGENS DE RJ.....: ' MEDIA-RJ "%"
            DISPLAY 'QTD. DE ACIDENTES TOTAIS DE RJ...: ' QTD-ACRJ
            DISPLAY 'QTD. DE CIDADES DE RJ PESQUISADAS: ' WS-QTD-CRJ
            .
       042-PROCESSAR-MG.

            COMPUTE WS-MEDIA-MG = WS-SOMA-MG / WS-CONT-MG
            MOVE WS-MEDIA-MG TO MEDIA-MG
            MOVE WS-QTD-AMG  TO QTD-ACMG
            DISPLAY '---------------------------------'
            DISPLAY 'MEDIA DAS PROCENTAGENS DE MG.....: ' MEDIA-MG "%"
            DISPLAY 'QTD. DE ACIDENTES TOTAIS DE MG...: ' QTD-ACMG
            DISPLAY 'QTD. DE CIDADES DE MG PESQUISADAS: ' WS-QTD-CMG
            .
       045-PROCESSAR-MAIOR.
            MOVE WS-QTD-MAIOR-CID TO QTD-MAIOR-CID
            DISPLAY '---------------------------------'
            DISPLAY 'CIDADE COM MAIOR QTD DE ACIDENTES: ' WS-CID-MAIOR
            DISPLAY 'QTD. DE ACIDENTES DESTA CIDADE...: '
               QTD-MAIOR-CID
            DISPLAY 'QTD. DE CIDADES PESQUISADAS......: ' WS-RGLIDO
            .
       047-PROCESSAR-MENOR.
            COMPUTE WS-M-PORC = (WS-QTD-M-OBITO / WS-QTD-M-ACID) * 100
            MOVE WS-M-PORC TO M-PORC
            DISPLAY '----------------------------------'
            DISPLAY 'CIDADE COM MENOR PORCENTAGEM DE OBITOS: '
                     WS-CID-MENOR
            DISPLAY 'PORCENTAGEM DE OBITOS DESTA CIDADE....: '
                     M-PORC '%'
            .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY ' *========================================*'
           DISPLAY ' *        FIM DO PROGRAMA CGPRG007           *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA CGPRG007 <-------------------*
