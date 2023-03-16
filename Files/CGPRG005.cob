       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG005.
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
           05  WS-CTLIDO              PIC 9(02).
           05  WS-MEDIA               PIC 9(02)V99.
           05  WS-TOTALF              PIC 99.
           05  WS-TOTALM              PIC 99.
           05  WS-MEDIAGERAL          PIC 99V99.
           05  WS-SOUTMEDIAGERAL      PIC Z9,99.
           05  WS-ABAIXOMEDIA         PIC 99.
           05  WS-PCTABAIXOMED        PIC 99V99.
           05  WS-SOMAMEDIAS          PIC 999V99.
           05  WS-SOUTMDMEDIAS        PIC 99,99.
       01  WS-SOUTPCT                 PIC Z9,99.
       01  FILLER                     PIC X(2) VALUE '% '.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-NUMERO-IN        PIC 9(04).
           05 WS-NOME-IN          PIC X(20).
           05 WS-SEXO-IN          PIC X(01).
           05 WS-IDADE-IN         PIC 9(02).
           05 WS-CURSO-IN         PIC X(12).
           05 WS-NOTA1-IN         PIC 9(02)V99.
           05 WS-NOTA2-IN         PIC 9(02)V99.
      *-----> SAIDA
       01  WS-REG-SYSOUT.
           05 WS-NUM              PIC 9(04).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-NOM              PIC X(20).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-SEX              PIC X(01).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-IDA              PIC Z9.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-CUR              PIC X(12).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-NT1              PIC Z9,99.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-NT2              PIC Z9,99.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-MED              PIC Z9,99.

       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-RSPRG002.

           DISPLAY "---------------------------------------------"
           DISPLAY "ATIVIDADE 5"
           DISPLAY "PEDRO"
           DISPLAY "CALCULO DA MEDIA DOS ALUNOS A PARTIR DA SYSIN"
           DISPLAY "---------------------------------------------"
           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           COMPUTE WS-MEDIAGERAL = WS-SOMAMEDIAS / WS-CTLIDO
           MOVE WS-MEDIAGERAL TO WS-SOUTMDMEDIAS
           COMPUTE WS-PCTABAIXOMED = WS-ABAIXOMEDIA / WS-CTLIDO
           COMPUTE WS-PCTABAIXOMED = WS-PCTABAIXOMED * 100
           MOVE WS-PCTABAIXOMED TO WS-SOUTPCT
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-SEXO-IN = 'F'
              ADD 1 TO WS-TOTALF
           END-IF
           IF WS-SEXO-IN = 'M'
              ADD 1 TO WS-TOTALM
           END-IF
           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

           COMPUTE WS-MEDIA = (WS-NOTA1-IN + WS-NOTA2-IN) / 2
           IF WS-MEDIA < 6
              ADD 1 TO WS-ABAIXOMEDIA
           END-IF
           ADD WS-MEDIA TO WS-SOMAMEDIAS
           MOVE WS-NUMERO-IN TO WS-NUM
           MOVE WS-NOME-IN TO WS-NOM
           MOVE WS-SEXO-IN TO WS-SEX
           MOVE WS-IDADE-IN TO WS-IDA
           MOVE WS-CURSO-IN TO WS-CUR
           MOVE WS-NOTA1-IN TO WS-NT1
           MOVE WS-NOTA2-IN TO WS-NT2
           MOVE WS-MEDIA TO WS-MED
           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS..: ' WS-CTLIDO
           DISPLAY ' * TOTAL MULHERES...: ' WS-TOTALF
           DISPLAY ' * TOTAL HOMENS.....: ' WS-TOTALM
           DISPLAY ' * MEDIA GERAL DOS ALUNOS......: ' WS-SOUTMDMEDIAS
           DISPLAY ' * TOTAL DE ALUNOS COM MEDIA < 6: ' WS-ABAIXOMEDIA
           DISPLAY ' * % DE ALUNOS COM MEDIA < 6....: ' WS-SOUTPCT
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA CGPRG005 <-------------------*
