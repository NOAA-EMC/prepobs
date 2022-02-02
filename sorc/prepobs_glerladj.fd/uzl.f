      SUBROUTINE UZL(U, ZM, TD, ZTM, CD, CH, Z0, FL)
C
C PURPOSE:
C               TO CALCULATE THE BULK AERODYNAMIC COEFFICIENTS FOR
C               MOMENTUM AND HEAT OVER A LAKE SURFACE AS FUNCTIONS
C               OF WIND SPEED AND AIR-SEA TEMPERATURE DIFFERENCE.
C
C ALGORITHM:
C               THERE IS AN OUTER ITERATION IN WHICH THE ROUGHNESS
C               LENGTH IS VARIED ACCORDING TO CHARNOCK'S FORMULA AND
C               AN INNER ITERATION IN WHICH THE STABILITY LENGTH
C               (MONIN-OBUKHOV LENGTH) IS VARIED ACCORDING TO THE
C               BUSINGER-DYER FORMULATION.  THE CONSTANT IN CHARMOCK'S
C               FORMULA IS CHOSEN SO THAT UNDER NEUTRAL CONDITIONS THE
C               10 M DRAG COEFFICIENT IS 0.0016.
C
C ARGUMENTS:
C  ON INPUT:
C               U - WIND SPEED (M / S)
C               ZM - ANEMOMETER HEIGHT (M)
C               TD - AIR-SEA TEMPERATURE DIFFERENCE (DEG K)
C               ZTM - THERMOMETER HEIGHT
C                     (INITIALLY ASSUMED EQUAL TO ANEMOMETER HEIGHT)
C  ON OUTPUT:
C               CD - BULK AERODYNAMIC COEFFICIENT FOR MOMENTUM
C               CH - BULK AERODYNAMIC COEFFICIENT FOR HEAT
C               Z0 - ROUGHNESS LENGTH (M)
C               FL - STABILITY LENGTH (M)
C
C HISTORY:
C               WRITTEN BY J. R. BENNETT AND J. D. BOYD, 1979, GLERL,
C               ANN, ARBOR, MI;  BASED ON A CONSTANT ROUGHNESS VERSION
C               WRITTEN BY PAUL LONG AND WILL SHAFFER OF THE TECHNIQUES
C               DEVELOPMENT LABORATORY, NOAA, SILVER SPRINGS, MD.
C
C               MODIFIED WHEN TRANSFERRED TO VAX FROM CDC IN 1984 TO WORK
C                CORRECTLY WITH 32 BIT REALS
C
C               MODIFIED 1/86 TO PREVENT MODIFICATION OF FIRST ARGUMENT IN
C                CALLING PARAMETERS (U) WHEN U < 0.001
C
      DATA C1, C2, C3 /.684E-4, 4.28E-3, -4.43E-4/
      DATA B1, B2, B3 /1.7989E-3, 4.865E-4, 3.9028E-5/
      EPS = .01
      UM = U
      IF (UM .LT. .001) UM = .001
      FK = .35
      TBAR = 278.
      ALPHA = 4.7
      BETA = .74
      GAMM = 15.
      GAMT = 9.
      UST1 = 0.04 * UM
      H = ZM
      !IF (TD.LT.0) then
      !   DTHETA=ABS(TD)
      !ELSE
      DTHETA = TD
      !ENDIF
      IF (ABS(DTHETA) .LT. 1.E-7) DTHETA = SIGN(1.E-7,DTHETA)
C      PRINT*, "INITIAL VALUES OF UM,UST1,TDIFF=",UM,UST1,TD
C
C INITIAL GUESS FOR Z0
C
      Z0 = .00624 * UST1 * UST1
      S = UM * UM * TBAR / (9.8*DTHETA)
C      print*, "INITIAL VALUE OF S=",S
      IF (ABS(S) .GT. 1.E6) S = SIGN(1.E6,S)
      X = ALOG(H/Z0)
C      PRINT*, "Initial value of X,H,Z0=",X,H,Z0
C
C       INITIAL GUESS FOR L
C
      FL = S / X
      DO 60 ITER = 1, 20
C      PRINT*,'OUTER ITERATION',ITER,' FL=',FL
        X = ALOG(H/Z0)
        IF (ABS(FL) .GT. 500.) FL = SIGN(500.,FL)
        IF (FL .GT. 0.) GO TO 20
C
C UNSTABLE SECTION (L LT 0 OR DT LT 0)
C
        FLI = 1. / FL
C
C ASSUME 5 ITERATIONS SUFFICIENT
C
        DO 10 I = 1, 5
C      PRINT*,'INNER ITERATION',I,' FL=',FL
          X1 = GAMT * FLI
          ARG1 = SQRT(1. - X1*H)
          ARG2 = SQRT(1. - X1*Z0)
          A = BETA * ALOG((ARG1 - 1.)*(ARG2 + 1.)/((ARG1 + 1.)*
     1    (ARG2 -0.999999)))
          X1 = GAMM * FLI
          ARG1 = (1. - X1*H) ** (.25)
          ARG2 = (1. - X1*Z0) ** (.25)
          B = ALOG((ARG1 - 1.)*(ARG2 + 1.)/((ARG1 + 1.)*
     1    (ARG2 - 0.999999))) +2. * (ATAN(ARG1) - ATAN(ARG2))
          FL = S * A / (B*B)
          IF (ABS(FL) .GT. 500.) FL = SIGN(500.,FL)
          FLI = 1. / FL
   10   CONTINUE
        GO TO 50
C
C STABLE SECTION
C
C TRY MILDLY STABLE-
C
   20   CONTINUE
        AA = X * X
        X1 = H - Z0
        BB = 9.4 * X1 * X - .74 * S * X
        CC = 4.7 * X1
        CC = CC * CC - CC * S
        ROOT = BB * BB - 4. * AA * CC
        IF (ROOT .LT. 0.) GO TO 30
        FL = (-BB + SQRT(ROOT)) / (2.*AA)
        IF (FL .LE. H) GO TO 30
        B = X + 4.7 * X1 / FL
        A = BETA * X + 4.7 * X1 / FL
        GO TO 50
C
C STRONGLY STABLE-
C
   30   CONTINUE
        IF (FL .LE. Z0) FL = Z0 + 1.E-5
        DO 40 I = 1, 5
          ARG1 = FL / Z0
          X1 = ALOG(ARG1)
          X2 = ALOG(H/FL)
          ARG1 = 1. - 1. / ARG1
          A = .74 * X1 + 4.7 * ARG1 + 5.44 * X2
          B = X1 + 4.7 * ARG1 + 5.7 * X2
          FL = A * S / (B*B)
          IF (FL .LE. Z0) FL = Z0 + 1.E-5
          IF (FL .GT. H) FL = H
   40   CONTINUE
C
C CALCULATE USTAR AND Z0NEW
C
   50   CONTINUE
        TSTAR = FK * DTHETA / A
        USTAR = FK * UM / B
        Z0NEW = .00624 * USTAR * USTAR
        IF (ITER .GT. 5 .AND. ABS((USTAR - UST1)/UST1) .LT. EPS)
     1     GO TO 80
        UST1 = USTAR
        Z0 = Z0NEW
   60 CONTINUE
C
C IF COME HERE, TOO MANY ITERATIONS (UGH - UGH)
C
      WRITE (6,70)
   70 FORMAT ('0TOO MANY ITERATIONS ON Z0 IN SUBROUTINE UZL - CHECK ',
     1       'METEOROLOGICAL DATA - PROGRAM TERMINATED')
      call w3tage('PREPOBS_GLERLADJ')
      CALL ERREXIT(34)
   80 CONTINUE
      Z0 = Z0NEW
      CD = (USTAR/UM) ** 2
      CH = FK * FK / (A*B)
   90 RETURN
      END
