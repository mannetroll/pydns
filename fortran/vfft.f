      SUBROUTINE VCFFTB (CR,CI,WR,WI,N,M,INC,JMP,WSAVE)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,N
      REAL CR(1+INC*(N-1)+JMP*(M-1)),CI(1+INC*(N-1)+JMP*(M-1))
      REAL WR(1+INC*(N-1)+JMP*(M-1)),WI(1+INC*(N-1)+JMP*(M-1))
      REAL WSAVE(15+2*N)
      IF (N .EQ. 1) RETURN
      CALL CFFTB1 (CR,CI,WR,WI,M,N,INC,JMP,WSAVE,WSAVE(2*N+1))
      RETURN
      END
      SUBROUTINE CFFTB1 (CR,CI,WR,WI,M,N,INC,JMP,WA,IFAC)
      INTEGER INC,JMP,M,N,IFAC(15)
      REAL CR(1+INC*(N-1)+JMP*(M-1)),CI(1+INC*(N-1)+JMP*(M-1))
      REAL WR(1+INC*(N-1)+JMP*(M-1)),WI(1+INC*(N-1)+JMP*(M-1))
      REAL WA(2*N)
      INTEGER NF,NA,L1,K1,IDO,L2,IDOT,IDL1,IP
      INTEGER IW,IX2,IX3,IX4,IX5,IX6,IX7
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IX5 = IX4+IDOT
         IX6 = IX5+IDOT
         IX7 = IX6+IDOT
         IF (IP.GT.9.OR.IP.EQ.7) THEN
           WRITE(6,*) 'FACTOR',IP,'NOT IMPLEMENTED'
           STOP
         ENDIF
         IF(NA .EQ. 0) THEN
           IF (IP .EQ. 4) CALL PASSB4 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3))
           IF (IP .EQ. 6) CALL PASSB6 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5))
           IF (IP .EQ. 5) CALL PASSB5 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4))
           IF (IP .EQ. 8) CALL PASSB8 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &         WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5),WA(IX6),WA(IX7))
           IF (IP .EQ. 3) CALL PASSB3 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2))
           IF (IP .EQ. 2) CALL PASSB2 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW))
         ELSE
           IF (IP .EQ. 4) CALL PASSB4 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3))
           IF (IP .EQ. 6) CALL PASSB6 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5))
           IF (IP .EQ. 5) CALL PASSB5 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4))
           IF (IP .EQ. 8) CALL PASSB8 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &         WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5),WA(IX6),WA(IX7))
           IF (IP .EQ. 3) CALL PASSB3 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2))
           IF (IP .EQ. 2) CALL PASSB2 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW))
         ENDIF
         NA=1-NA
         L1 = L2
         IW = IW+(IP-1)*IDOT
  116 CONTINUE
      IF (NA .EQ. 0) RETURN
      DO 117 I=0,N-1
      DO 117 J=0,M-1
         CR(1+INC*I+JMP*J) = WR(1+INC*I+JMP*J)
         CI(1+INC*I+JMP*J) = WI(1+INC*I+JMP*J)
  117 CONTINUE
      RETURN
      END

      SUBROUTINE PASSB2 (CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,WA1)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TR2,TI2
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*2*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          CHR(IU,1) = CCR(II,1)+CCR(II,2)
          CHR(IU,2) = CCR(II,1)-CCR(II,2)
          CHI(IU,1) = CCI(II,1)+CCI(II,2)
          CHI(IU,2) = CCI(II,1)-CCI(II,2)
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*2*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          CHR(IU,1) = CCR(II,1)+CCR(II,2)
          TR2 = CCR(II,1)-CCR(II,2)
          CHI(IU,1) = CCI(II,1)+CCI(II,2)
          TI2 = CCI(II,1)-CCI(II,2)
          CHI(IU,2) = WA1(2*I-1)*TI2+WA1(2*I)*TR2
          CHR(IU,2) = WA1(2*I-1)*TR2-WA1(2*I)*TI2
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSB3 (CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,WA1,WA2)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TAUR,TAUI
      REAL TR2,TI2,CR2,CI2,CR3,CI3,DR2,DI2,DR3,DI3
      DATA TAUR,TAUI /-.5,.866025403784439/
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*3*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TR2 = CCR(II,2)+CCR(II,3)
          CR2 = CCR(II,1)+TAUR*TR2
          CHR(IU,1) = CCR(II,1)+TR2
          TI2 = CCI(II,2)+CCI(II,3)
          CI2 = CCI(II,1)+TAUR*TI2
          CHI(IU,1) = CCI(II,1)+TI2
          CR3 = TAUI*(CCR(II,2)-CCR(II,3))
          CI3 = TAUI*(CCI(II,2)-CCI(II,3))
          CHR(IU,2) = CR2-CI3
          CHR(IU,3) = CR2+CI3
          CHI(IU,2) = CI2+CR3
          CHI(IU,3) = CI2-CR3
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*3*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TR2 = CCR(II,2)+CCR(II,3)
          CR2 = CCR(II,1)+TAUR*TR2
          CHR(IU,1) = CCR(II,1)+TR2
          TI2 = CCI(II,2)+CCI(II,3)
          CI2 = CCI(II,1)+TAUR*TI2
          CHI(IU,1) = CCI(II,1)+TI2
          CR3 = TAUI*(CCR(II,2)-CCR(II,3))
          CI3 = TAUI*(CCI(II,2)-CCI(II,3))
          DR2 = CR2-CI3
          DR3 = CR2+CI3
          DI2 = CI2+CR3
          DI3 = CI2-CR3
          CHI(IU,2) = WA1(2*I-1)*DI2+WA1(2*I)*DR2
          CHR(IU,2) = WA1(2*I-1)*DR2-WA1(2*I)*DI2
          CHI(IU,3) = WA2(2*I-1)*DI3+WA2(2*I)*DR3
          CHR(IU,3) = WA2(2*I-1)*DR3-WA2(2*I)*DI3
  103   CONTINUE
      ENDIF
      RETURN
      END


      SUBROUTINE PASSB4 (CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,WA1,WA2,WA3)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TR1,TI1,TR2,TI2,TR3,TI3,TR4,TI4,CR3,CI3
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*4*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TI1 = CCI(II,1)-CCI(II,3)
          TI2 = CCI(II,1)+CCI(II,3)
          TI3 = CCI(II,2)+CCI(II,4)
          TR4 = CCI(II,4)-CCI(II,2)
          CHI(IU,1) = TI2+TI3
          CHI(IU,3) = TI2-TI3
          CHR(IU,2) = CCR(II,1)-CCR(II,3)+TR4
          CHR(IU,4) = CCR(II,1)-CCR(II,3)-TR4
C          TR1 = CCR(II,1)-CCR(II,3)
C          TR2 = CCR(II,1)+CCR(II,3)
C          TI4 = CCR(II,2)-CCR(II,4)
C          TR3 = CCR(II,2)+CCR(II,4)
          CHI(IU,2) = TI1+(CCR(II,2)-CCR(II,4))
          CHI(IU,4) = TI1-(CCR(II,2)-CCR(II,4))
          CHR(IU,1) = CCR(II,1)+CCR(II,3)+(CCR(II,2)+CCR(II,4))
          CHR(IU,3) = CCR(II,1)+CCR(II,3)-(CCR(II,2)+CCR(II,4))
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*4*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TR1 = CCR(II,1)-CCR(II,3)
          TR2 = CCR(II,1)+CCR(II,3)
          TR3 = CCR(II,2)+CCR(II,4)
          CR3 = TR2-TR3
          TI4 = CCR(II,2)-CCR(II,4)
          CHR(IU,1) = TR2+TR3
          TI1 = CCI(II,1)-CCI(II,3)
          TI2 = CCI(II,1)+CCI(II,3)
          TI3 = CCI(II,2)+CCI(II,4)
          TR4 = CCI(II,4)-CCI(II,2)
          CHI(IU,1) = TI2+TI3
          CI3 = TI2-TI3
          CHR(IU,3) = WA2(2*I-1)*CR3-WA2(2*I)*CI3
          CHI(IU,3) = WA2(2*I-1)*CI3+WA2(2*I)*CR3
C          CR2 = TR1+TR4
C          CR4 = TR1-TR4
C          CI2 = TI1+TI4
C          CI4 = TI1-TI4
          CHR(IU,2) = WA1(2*I-1)*(TR1+TR4)-WA1(2*I)*(TI1+TI4)
          CHI(IU,2) = WA1(2*I-1)*(TI1+TI4)+WA1(2*I)*(TR1+TR4)
          CHR(IU,4) = WA3(2*I-1)*(TR1-TR4)-WA3(2*I)*(TI1-TI4)
          CHI(IU,4) = WA3(2*I-1)*(TI1-TI4)+WA3(2*I)*(TR1-TR4)
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSB5(CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,
     & WA1,WA2,WA3,WA4)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO),WA4(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TR11,TI11,TR12,TI12
      REAL TR2,TI2,TR3,TI3,TR4,TI4,TR5,TI5,CR2,CI2,CR3,CI3,CR4,CI4
      REAL CR5,CI5
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*5*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TI5 = CCI(II,2)-CCI(II,5)
          TI2 = CCI(II,2)+CCI(II,5)
          TI4 = CCI(II,3)-CCI(II,4)
          TI3 = CCI(II,3)+CCI(II,4)
          CHI(IU,1) = CCI(II,1)+TI2+TI3
          TR5 = CCR(II,2)-CCR(II,5)
          TR2 = CCR(II,2)+CCR(II,5)
          TR4 = CCR(II,3)-CCR(II,4)
          TR3 = CCR(II,3)+CCR(II,4)
          CHR(IU,1) = CCR(II,1)+TR2+TR3
          CR2 = CCR(II,1)+TR11*TR2+TR12*TR3
          CR3 = CCR(II,1)+TR12*TR2+TR11*TR3
          CI2 = CCI(II,1)+TR11*TI2+TR12*TI3
          CI3 = CCI(II,1)+TR12*TI2+TR11*TI3
          CR5 = TI11*TR5+TI12*TR4
          CR4 = TI12*TR5-TI11*TR4
          CI5 = TI11*TI5+TI12*TI4
          CI4 = TI12*TI5-TI11*TI4
          CHR(IU,2) = CR2-CI5
          CHR(IU,5) = CR2+CI5
          CHI(IU,2) = CI2+CR5
          CHI(IU,5) = CI2-CR5
          CHR(IU,3) = CR3-CI4
          CHR(IU,4) = CR3+CI4
          CHI(IU,3) = CI3+CR4
          CHI(IU,4) = CI3-CR4
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*5*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TI5 = CCI(II,2)-CCI(II,5)
          TI2 = CCI(II,2)+CCI(II,5)
          TI4 = CCI(II,3)-CCI(II,4)
          TI3 = CCI(II,3)+CCI(II,4)
          TR5 = CCR(II,2)-CCR(II,5)
          TR2 = CCR(II,2)+CCR(II,5)
          TR4 = CCR(II,3)-CCR(II,4)
          TR3 = CCR(II,3)+CCR(II,4)
          CHR(IU,1) = CCR(II,1)+TR2+TR3
          CHI(IU,1) = CCI(II,1)+TI2+TI3
          CR2 = CCR(II,1)+TR11*TR2+TR12*TR3
          CR3 = CCR(II,1)+TR12*TR2+TR11*TR3
          CI2 = CCI(II,1)+TR11*TI2+TR12*TI3
          CI3 = CCI(II,1)+TR12*TI2+TR11*TI3
          CR5 = TI11*TR5+TI12*TR4
          CR4 = TI12*TR5-TI11*TR4
          CI5 = TI11*TI5+TI12*TI4
          CI4 = TI12*TI5-TI11*TI4
C          DR3 = CR3-CI4
C          DR4 = CR3+CI4
C          DI3 = CI3+CR4
C          DI4 = CI3-CR4
          CHR(IU,3) = WA2(2*I-1)*(CR3-CI4)-WA2(2*I)*(CI3+CR4)
          CHI(IU,3) = WA2(2*I-1)*(CI3+CR4)+WA2(2*I)*(CR3-CI4)
          CHR(IU,4) = WA3(2*I-1)*(CR3+CI4)-WA3(2*I)*(CI3-CR4)
          CHI(IU,4) = WA3(2*I-1)*(CI3-CR4)+WA3(2*I)*(CR3+CI4)
C          DR5 = CR2+CI5
C          DR2 = CR2-CI5
C          DI5 = CI2-CR5
C          DI2 = CI2+CR5
          CHR(IU,2) = WA1(2*I-1)*(CR2-CI5)-WA1(2*I)*(CI2+CR5)
          CHI(IU,2) = WA1(2*I-1)*(CI2+CR5)+WA1(2*I)*(CR2-CI5)
          CHR(IU,5) = WA4(2*I-1)*(CR2+CI5)-WA4(2*I)*(CI2-CR5)
          CHI(IU,5) = WA4(2*I-1)*(CI2-CR5)+WA4(2*I)*(CR2+CI5)
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSB6(CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,
     & WA1,WA2,WA3,WA4,WA5)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO),WA4(2*IDO),WA5(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TAUR,TAUI
      REAL TR2,TI2,TR5,TI5
      REAL CR2,CI2,CR3,CI3,CR5,CI5,CR6,CI6
      REAL DR1,DI1,DR2,DI2,DR3,DI3,DR4,DI4,DR5,DI5,DR6,DI6
      REAL ER2,EI2,ER3,EI3,ER4,EI4,ER5,EI5,ER6,EI6
      DATA TAUR,TAUI /-.5,.866025403784439/
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*6*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TR2 = CCR(II,3)+CCR(II,5)
          CR3 = TAUI*(CCR(II,3)-CCR(II,5))
          CR2 = CCR(II,1)+TAUR*TR2
          DR1 = CCR(II,1)+TR2
          TI2 = CCI(II,3)+CCI(II,5)
          CI3 = TAUI*(CCI(II,3)-CCI(II,5))
          CI2 = CCI(II,1)+TAUR*TI2
          DI1 = CCI(II,1)+TI2
C          DR3 = CR2-CI3
C          DR5 = CR2+CI3
          TR5 = CCR(II,6)+CCR(II,2)
          CR6 = TAUI*(CCR(II,6)-CCR(II,2))
          CR5 = CCR(II,4)+TAUR*TR5
          DR2 = CCR(II,4)+TR5
          TI5 = CCI(II,6)+CCI(II,2)
          CI6 = TAUI*(CCI(II,6)-CCI(II,2))
          CI5 = CCI(II,4)+TAUR*TI5
          DI2 = CCI(II,4)+TI5
          CHI(IU,1) = DI1+DI2
          CHI(IU,4) = DI1-DI2
          CHR(IU,1) = DR1+DR2
          CHR(IU,4) = DR1-DR2
C          DR4 = CR5-CI6
C          DR6 = CR5+CI6
C          DI3 = CI2+CR3
C          DI5 = CI2-CR3
C          DI4 = CI5+CR6
C          DI6 = CI5-CR6
          CHI(IU,5) = CI2+CR3+(CI5+CR6)
          CHI(IU,2) = CI2+CR3-(CI5+CR6)
          CHI(IU,3) = CI2-CR3+(CI5-CR6)
          CHI(IU,6) = CI2-CR3-(CI5-CR6)
          CHR(IU,5) = CR2-CI3+(CR5-CI6)
          CHR(IU,2) = CR2-CI3-(CR5-CI6)
          CHR(IU,3) = CR2+CI3+(CR5+CI6)
          CHR(IU,6) = CR2+CI3-(CR5+CI6)
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*6*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TR2 = CCR(II,3)+CCR(II,5)
          CR3 = TAUI*(CCR(II,3)-CCR(II,5))
          CR2 = CCR(II,1)+TAUR*TR2
          DR1 = CCR(II,1)+TR2
          TI2 = CCI(II,3)+CCI(II,5)
          CI3 = TAUI*(CCI(II,3)-CCI(II,5))
          CI2 = CCI(II,1)+TAUR*TI2
          DI1 = CCI(II,1)+TI2
          DR3 = CR2-CI3
          DR5 = CR2+CI3
          DI3 = CI2+CR3
          DI5 = CI2-CR3
          TR5 = CCR(II,6)+CCR(II,2)
          CR6 = TAUI*(CCR(II,6)-CCR(II,2))
          CR5 = CCR(II,4)+TAUR*TR5
          DR2 = CCR(II,4)+TR5
          TI5 = CCI(II,6)+CCI(II,2)
          CI6 = TAUI*(CCI(II,6)-CCI(II,2))
          CI5 = CCI(II,4)+TAUR*TI5
          DI2 = CCI(II,4)+TI5
          CHR(IU,1) = DR1+DR2
          ER4 = DR1-DR2
          CHI(IU,1) = DI1+DI2
          EI4 = DI1-DI2
          DR4 = CR5-CI6
          DR6 = CR5+CI6
          DI4 = CI5+CR6
          DI6 = CI5-CR6
          CHR(IU,4) = WA3(2*I-1)*ER4-WA3(2*I)*EI4
          CHI(IU,4) = WA3(2*I-1)*EI4+WA3(2*I)*ER4
          ER5 = DR3+DR4
          ER2 = DR3-DR4
          EI5 = DI3+DI4
          EI2 = DI3-DI4
          CHR(IU,2) = WA1(2*I-1)*ER2-WA1(2*I)*EI2
          CHI(IU,2) = WA1(2*I-1)*EI2+WA1(2*I)*ER2
          CHR(IU,5) = WA4(2*I-1)*ER5-WA4(2*I)*EI5
          CHI(IU,5) = WA4(2*I-1)*EI5+WA4(2*I)*ER5
          ER3 = DR5+DR6
          ER6 = DR5-DR6
          EI3 = DI5+DI6
          EI6 = DI5-DI6
          CHR(IU,3) = WA2(2*I-1)*ER3-WA2(2*I)*EI3
          CHI(IU,3) = WA2(2*I-1)*EI3+WA2(2*I)*ER3
          CHR(IU,6) = WA5(2*I-1)*ER6-WA5(2*I)*EI6
          CHI(IU,6) = WA5(2*I-1)*EI6+WA5(2*I)*ER6
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSB8(CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,
     & WA1,WA2,WA3,WA4,WA5,WA6,WA7)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO),WA4(2*IDO),WA5(2*IDO)
      REAL WA6(2*IDO),WA7(2*IDO)
      INTEGER I,J,K,II,IU
      REAL W
      REAL TR1,TI1,TR2,TI2,TR3,TI3,TR4,TI4
      REAL TR5,TI5,TR6,TI6,TR7,TI7,TR8,TI8
      REAL SR1,SI1,SR2,SI2,SR3,SI3,SR4,SI4
      REAL SR5,SI5,SR6,SI6,SR7,SI7,SR8,SI8
      REAL QR1,QI1,QR2,QI2,CR2,CI2,CR3,CI3,CR4,CI4
      REAL CR5,CI5,CR6,CI6,CR7,CI7,CR8,CI8
      DATA W/.7071067811865476/    
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*8*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          SR1 = CCR(II,1)+CCR(II,5)
          SR2 = CCR(II,1)-CCR(II,5)
          SR3 = CCR(II,2)+CCR(II,6)
          SR4 = CCR(II,2)-CCR(II,6)
          SR5 = CCR(II,3)+CCR(II,7)
          SR6 = CCR(II,3)-CCR(II,7)
          SR7 = CCR(II,4)+CCR(II,8)
          SR8 = CCR(II,4)-CCR(II,8)
          TR1 = SR1+SR5
          TR2 = SR1-SR5
          TR3 = SR3+SR7
          TR4 = SR3-SR7
          CHR(IU,1) = TR1+TR3 
          CHR(IU,5) = TR1-TR3
          SI1 = CCI(II,1)+CCI(II,5)
          SI2 = CCI(II,1)-CCI(II,5)
          SI3 = CCI(II,2)+CCI(II,6)
          SI4 = CCI(II,2)-CCI(II,6)
          SI5 = CCI(II,3)+CCI(II,7)
          SI6 = CCI(II,3)-CCI(II,7)
          SI7 = CCI(II,4)+CCI(II,8)
          SI8 = CCI(II,4)-CCI(II,8)
          TI1 = SI1+SI5
          TI2 = SI1-SI5
          TI3 = SI3+SI7
          TI4 = SI3-SI7
          CHI(IU,1) = TI1+TI3 
          CHI(IU,5) = TI1-TI3
          CHR(IU,3) = TR2-TI4
          CHR(IU,7) = TR2+TI4
          CHI(IU,3) = TI2+TR4
          CHI(IU,7) = TI2-TR4
          TR5 = SR2-SI6
          TR6 = SR2+SI6
          TR7 = W*(SR4-SI4)
          TI7 = W*(SI4+SR4)
          TR8 = W*(SR8-SI8)
          TI8 = W*(SI8+SR8)
          TI5 = SI2+SR6
          TI6 = SI2-SR6
          QI1 = TI7+TR8
          QR2 = TR8-TI7
          QR1 = TR7-TI8
          QI2 = TI8+TR7
          CHR(IU,2) = TR5+QR1
          CHR(IU,6) = TR5-QR1
          CHI(IU,2) = TI5+QI1
          CHI(IU,6) = TI5-QI1
          CHR(IU,4) = TR6+QR2
          CHR(IU,8) = TR6-QR2
          CHI(IU,4) = TI6+QI2
          CHI(IU,8) = TI6-QI2
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*8*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          SR1 = CCR(II,1)+CCR(II,5)
          SR2 = CCR(II,1)-CCR(II,5)
          SR3 = CCR(II,2)+CCR(II,6)
          SR4 = CCR(II,2)-CCR(II,6)
          SR5 = CCR(II,3)+CCR(II,7)
          SR6 = CCR(II,3)-CCR(II,7)
          SR7 = CCR(II,4)+CCR(II,8)
          SR8 = CCR(II,4)-CCR(II,8)
          TR1 = SR1+SR5
          TR2 = SR1-SR5
          TR3 = SR3+SR7
          TR4 = SR3-SR7
          CHR(IU,1) = TR1+TR3 
          CR5 = TR1-TR3
          SI1 = CCI(II,1)+CCI(II,5)
          SI2 = CCI(II,1)-CCI(II,5)
          SI3 = CCI(II,2)+CCI(II,6)
          SI4 = CCI(II,2)-CCI(II,6)
          SI5 = CCI(II,3)+CCI(II,7)
          SI6 = CCI(II,3)-CCI(II,7)
          SI7 = CCI(II,4)+CCI(II,8)
          SI8 = CCI(II,4)-CCI(II,8)
          TI1 = SI1+SI5
          TI2 = SI1-SI5
          TI3 = SI3+SI7
          TI4 = SI3-SI7
          CHI(IU,1) = TI1+TI3 
          CI5 = TI1-TI3
          CHR(IU,5) = WA4(2*I-1)*CR5-WA4(2*I)*CI5
          CHI(IU,5) = WA4(2*I-1)*CI5+WA4(2*I)*CR5
          CR3 = TR2-TI4
          CR7 = TR2+TI4
          CI3 = TI2+TR4
          CI7 = TI2-TR4
          CHR(IU,3) = WA2(2*I-1)*CR3-WA2(2*I)*CI3
          CHI(IU,3) = WA2(2*I-1)*CI3+WA2(2*I)*CR3
          CHR(IU,7) = WA6(2*I-1)*CR7-WA6(2*I)*CI7
          CHI(IU,7) = WA6(2*I-1)*CI7+WA6(2*I)*CR7
          TR5 = SR2-SI6
          TR6 = SR2+SI6
          TR7 = W*(SR4-SI4)
          TI7 = W*(SI4+SR4)
          TR8 = W*(SR8-SI8)
          TI8 = W*(SI8+SR8)
          TI5 = SI2+SR6
          TI6 = SI2-SR6
          QI1 = TI7+TR8
          QR2 = TR8-TI7
          QR1 = TR7-TI8
          QI2 = TI8+TR7
          CR2 = TR5+QR1
          CR6 = TR5-QR1
          CI2 = TI5+QI1
          CI6 = TI5-QI1
          CHR(IU,2) = WA1(2*I-1)*CR2-WA1(2*I)*CI2
          CHI(IU,2) = WA1(2*I-1)*CI2+WA1(2*I)*CR2
          CHR(IU,6) = WA5(2*I-1)*CR6-WA5(2*I)*CI6
          CHI(IU,6) = WA5(2*I-1)*CI6+WA5(2*I)*CR6
          CR4 = TR6+QR2
          CR8 = TR6-QR2
          CI4 = TI6+QI2
          CI8 = TI6-QI2
          CHR(IU,4) = WA3(2*I-1)*CR4-WA3(2*I)*CI4
          CHI(IU,4) = WA3(2*I-1)*CI4+WA3(2*I)*CR4
          CHR(IU,8) = WA7(2*I-1)*CR8-WA7(2*I)*CI8
          CHI(IU,8) = WA7(2*I-1)*CI8+WA7(2*I)*CR8
  103   CONTINUE
      ENDIF
      RETURN
      END




      SUBROUTINE VCFFTF (CR,CI,WR,WI,N,M,INC,JMP,WSAVE)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,N
      REAL CR(1+INC*(N-1)+JMP*(M-1)),CI(1+INC*(N-1)+JMP*(M-1))
      REAL WR(1+INC*(N-1)+JMP*(M-1)),WI(1+INC*(N-1)+JMP*(M-1))
      REAL WSAVE(15+2*N)
      IF (N .EQ. 1) RETURN
      CALL CFFTF1 (CR,CI,WR,WI,M,N,INC,JMP,WSAVE,WSAVE(2*N+1))
      RETURN
      END
      SUBROUTINE CFFTF1 (CR,CI,WR,WI,M,N,INC,JMP,WA,IFAC)
      INTEGER INC,JMP,M,N,IFAC(15)
      REAL CR(1+INC*(N-1)+JMP*(M-1)),CI(1+INC*(N-1)+JMP*(M-1))
      REAL WR(1+INC*(N-1)+JMP*(M-1)),WI(1+INC*(N-1)+JMP*(M-1))
      REAL WA(2*N)
      INTEGER NF,NA,L1,K1,IDO,L2,IDOT,IDL1,IP
      INTEGER IW,IX2,IX3,IX4,IX5,IX6,IX7
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IX5 = IX4+IDOT
         IX6 = IX5+IDOT
         IX7 = IX6+IDOT
         IF (IP.GT.9.OR.IP.EQ.7) THEN
           WRITE(6,*) 'FACTOR',IP,'NOT IMPLEMENTED'
           STOP
         ENDIF
         IF(NA .EQ. 0) THEN
           IF (IP .EQ. 4) CALL PASSF4 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3))
           IF (IP .EQ. 6) CALL PASSF6 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5))
           IF (IP .EQ. 5) CALL PASSF5 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4))
           IF (IP .EQ. 8) CALL PASSF8 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &         WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5),WA(IX6),WA(IX7))
           IF (IP .EQ. 3) CALL PASSF3 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2))
           IF (IP .EQ. 2) CALL PASSF2 (CR,CI,WR,WI,INC,JMP,M,IDO,L1,
     &                    WA(IW))
         ELSE
           IF (IP .EQ. 4) CALL PASSF4 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3))
           IF (IP .EQ. 6) CALL PASSF6 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5))
           IF (IP .EQ. 5) CALL PASSF5 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2),WA(IX3),WA(IX4))
           IF (IP .EQ. 8) CALL PASSF8 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &         WA(IW),WA(IX2),WA(IX3),WA(IX4),WA(IX5),WA(IX6),WA(IX7))
           IF (IP .EQ. 3) CALL PASSF3 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW),WA(IX2))
           IF (IP .EQ. 2) CALL PASSF2 (WR,WI,CR,CI,INC,JMP,M,IDO,L1,
     &                    WA(IW))
         ENDIF
         NA=1-NA
         L1 = L2
         IW = IW+(IP-1)*IDOT
  116 CONTINUE
      IF (NA .EQ. 0) RETURN
      DO 117 I=0,N-1
      DO 117 J=0,M-1
         CR(1+INC*I+JMP*J) = WR(1+INC*I+JMP*J)
         CI(1+INC*I+JMP*J) = WI(1+INC*I+JMP*J)
  117 CONTINUE
      RETURN
      END

      SUBROUTINE PASSF2 (CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,WA1)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TR2,TI2
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*2*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          CHR(IU,1) = CCR(II,1)+CCR(II,2)
          CHR(IU,2) = CCR(II,1)-CCR(II,2)
          CHI(IU,1) = CCI(II,1)+CCI(II,2)
          CHI(IU,2) = CCI(II,1)-CCI(II,2)
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*2*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          CHR(IU,1) = CCR(II,1)+CCR(II,2)
          TR2 = CCR(II,1)-CCR(II,2)
          CHI(IU,1) = CCI(II,1)+CCI(II,2)
          TI2 = CCI(II,1)-CCI(II,2)
          CHI(IU,2) = WA1(2*I-1)*TI2-WA1(2*I)*TR2
          CHR(IU,2) = WA1(2*I-1)*TR2+WA1(2*I)*TI2
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSF3 (CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,WA1,WA2)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TAUR,TAUI
      REAL TR2,TI2,CR2,CI2,CR3,CI3,DR2,DI2,DR3,DI3
      DATA TAUR,TAUI /-.5,-.866025403784439/
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*3*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TR2 = CCR(II,2)+CCR(II,3)
          CR2 = CCR(II,1)+TAUR*TR2
          CHR(IU,1) = CCR(II,1)+TR2
          TI2 = CCI(II,2)+CCI(II,3)
          CI2 = CCI(II,1)+TAUR*TI2
          CHI(IU,1) = CCI(II,1)+TI2
          CR3 = TAUI*(CCR(II,2)-CCR(II,3))
          CI3 = TAUI*(CCI(II,2)-CCI(II,3))
          CHR(IU,2) = CR2-CI3
          CHR(IU,3) = CR2+CI3
          CHI(IU,2) = CI2+CR3
          CHI(IU,3) = CI2-CR3
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*3*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TR2 = CCR(II,2)+CCR(II,3)
          CR2 = CCR(II,1)+TAUR*TR2
          CHR(IU,1) = CCR(II,1)+TR2
          TI2 = CCI(II,2)+CCI(II,3)
          CI2 = CCI(II,1)+TAUR*TI2
          CHI(IU,1) = CCI(II,1)+TI2
          CR3 = TAUI*(CCR(II,2)-CCR(II,3))
          CI3 = TAUI*(CCI(II,2)-CCI(II,3))
          DR2 = CR2-CI3
          DR3 = CR2+CI3
          DI2 = CI2+CR3
          DI3 = CI2-CR3
          CHI(IU,2) = WA1(2*I-1)*DI2-WA1(2*I)*DR2
          CHR(IU,2) = WA1(2*I-1)*DR2+WA1(2*I)*DI2
          CHI(IU,3) = WA2(2*I-1)*DI3-WA2(2*I)*DR3
          CHR(IU,3) = WA2(2*I-1)*DR3+WA2(2*I)*DI3
  103   CONTINUE
      ENDIF
      RETURN
      END


      SUBROUTINE PASSF4 (CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,WA1,WA2,WA3)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TR1,TI1,TR2,TI2,TR3,TI3,TR4,TI4,CR3,CI3
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*4*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TI1 = CCI(II,1)-CCI(II,3)
          TI2 = CCI(II,1)+CCI(II,3)
          TI3 = CCI(II,2)+CCI(II,4)
          CHI(IU,1) = TI2+TI3
          CHI(IU,3) = TI2-TI3
          TR4 = CCI(II,2)-CCI(II,4)
          CHR(IU,2) = CCR(II,1)-CCR(II,3)+TR4
          CHR(IU,4) = CCR(II,1)-CCR(II,3)-TR4
C          TR1 = CCR(II,1)-CCR(II,3)
C          TR2 = CCR(II,1)+CCR(II,3)
C          TI4 = CCR(II,4)-CCR(II,2)
C          TR3 = CCR(II,2)+CCR(II,4)
          CHI(IU,2) = (CCI(II,1)-CCI(II,3))+(CCR(II,4)-CCR(II,2))
          CHI(IU,4) = (CCI(II,1)-CCI(II,3))-(CCR(II,4)-CCR(II,2))
          CHR(IU,1) = CCR(II,1)+CCR(II,3)+(CCR(II,2)+CCR(II,4))
          CHR(IU,3) = CCR(II,1)+CCR(II,3)-(CCR(II,2)+CCR(II,4))
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*4*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TR1 = CCR(II,1)-CCR(II,3)
          TR2 = CCR(II,1)+CCR(II,3)
          TR3 = CCR(II,2)+CCR(II,4)
          CR3 = TR2-TR3
          TI4 = CCR(II,4)-CCR(II,2)
          CHR(IU,1) = TR2+TR3
          TI1 = CCI(II,1)-CCI(II,3)
          TI2 = CCI(II,1)+CCI(II,3)
          TI3 = CCI(II,2)+CCI(II,4)
          TR4 = CCI(II,2)-CCI(II,4)
          CHI(IU,1) = TI2+TI3
          CI3 = TI2-TI3
          CHR(IU,3) = WA2(2*I-1)*CR3+WA2(2*I)*CI3
          CHI(IU,3) = WA2(2*I-1)*CI3-WA2(2*I)*CR3
C          CR2 = TR1+TR4
C          CR4 = TR1-TR4
C          CI2 = TI1+TI4
C          CI4 = TI1-TI4
          CHR(IU,2) = WA1(2*I-1)*(TR1+TR4)+WA1(2*I)*(TI1+TI4)
          CHI(IU,2) = WA1(2*I-1)*(TI1+TI4)-WA1(2*I)*(TR1+TR4)
          CHR(IU,4) = WA3(2*I-1)*(TR1-TR4)+WA3(2*I)*(TI1-TI4)
          CHI(IU,4) = WA3(2*I-1)*(TI1-TI4)-WA3(2*I)*(TR1-TR4)
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSF5(CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,
     & WA1,WA2,WA3,WA4)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO),WA4(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TR11,TI11,TR12,TI12
      REAL TR2,TI2,TR3,TI3,TR4,TI4,TR5,TI5,CR2,CI2,CR3,CI3,CR4,CI4
      REAL CR5,CI5
      DATA TR11,TI11,TR12,TI12 /.309016994374947,-.951056516295154,
     1-.809016994374947,-.587785252292473/
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*5*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TI5 = CCI(II,2)-CCI(II,5)
          TI2 = CCI(II,2)+CCI(II,5)
          TI4 = CCI(II,3)-CCI(II,4)
          TI3 = CCI(II,3)+CCI(II,4)
          CHI(IU,1) = CCI(II,1)+TI2+TI3
          TR5 = CCR(II,2)-CCR(II,5)
          TR2 = CCR(II,2)+CCR(II,5)
          TR4 = CCR(II,3)-CCR(II,4)
          TR3 = CCR(II,3)+CCR(II,4)
          CHR(IU,1) = CCR(II,1)+TR2+TR3
          CR2 = CCR(II,1)+TR11*TR2+TR12*TR3
          CR3 = CCR(II,1)+TR12*TR2+TR11*TR3
          CI2 = CCI(II,1)+TR11*TI2+TR12*TI3
          CI3 = CCI(II,1)+TR12*TI2+TR11*TI3
          CR5 = TI11*TR5+TI12*TR4
          CR4 = TI12*TR5-TI11*TR4
          CI5 = TI11*TI5+TI12*TI4
          CI4 = TI12*TI5-TI11*TI4
          CHR(IU,2) = CR2-CI5
          CHR(IU,5) = CR2+CI5
          CHI(IU,2) = CI2+CR5
          CHI(IU,5) = CI2-CR5
          CHR(IU,3) = CR3-CI4
          CHR(IU,4) = CR3+CI4
          CHI(IU,3) = CI3+CR4
          CHI(IU,4) = CI3-CR4
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*5*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TI5 = CCI(II,2)-CCI(II,5)
          TI2 = CCI(II,2)+CCI(II,5)
          TI4 = CCI(II,3)-CCI(II,4)
          TI3 = CCI(II,3)+CCI(II,4)
          TR5 = CCR(II,2)-CCR(II,5)
          TR2 = CCR(II,2)+CCR(II,5)
          TR4 = CCR(II,3)-CCR(II,4)
          TR3 = CCR(II,3)+CCR(II,4)
          CHR(IU,1) = CCR(II,1)+TR2+TR3
          CHI(IU,1) = CCI(II,1)+TI2+TI3
          CR2 = CCR(II,1)+TR11*TR2+TR12*TR3
          CR3 = CCR(II,1)+TR12*TR2+TR11*TR3
          CI2 = CCI(II,1)+TR11*TI2+TR12*TI3
          CI3 = CCI(II,1)+TR12*TI2+TR11*TI3
          CR5 = TI11*TR5+TI12*TR4
          CR4 = TI12*TR5-TI11*TR4
          CI5 = TI11*TI5+TI12*TI4
          CI4 = TI12*TI5-TI11*TI4
C          DR3 = CR3-CI4
C          DR4 = CR3+CI4
C          DI3 = CI3+CR4
C          DI4 = CI3-CR4
          CHR(IU,3) = WA2(2*I-1)*(CR3-CI4)+WA2(2*I)*(CI3+CR4)
          CHI(IU,3) = WA2(2*I-1)*(CI3+CR4)-WA2(2*I)*(CR3-CI4)
          CHR(IU,4) = WA3(2*I-1)*(CR3+CI4)+WA3(2*I)*(CI3-CR4)
          CHI(IU,4) = WA3(2*I-1)*(CI3-CR4)-WA3(2*I)*(CR3+CI4)
C          DR5 = CR2+CI5
C          DR2 = CR2-CI5
C          DI5 = CI2-CR5
C          DI2 = CI2+CR5
          CHR(IU,2) = WA1(2*I-1)*(CR2-CI5)+WA1(2*I)*(CI2+CR5)
          CHI(IU,2) = WA1(2*I-1)*(CI2+CR5)-WA1(2*I)*(CR2-CI5)
          CHR(IU,5) = WA4(2*I-1)*(CR2+CI5)+WA4(2*I)*(CI2-CR5)
          CHI(IU,5) = WA4(2*I-1)*(CI2-CR5)-WA4(2*I)*(CR2+CI5)
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSF6(CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,
     & WA1,WA2,WA3,WA4,WA5)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO),WA4(2*IDO),WA5(2*IDO)
      INTEGER I,J,K,II,IU
      REAL TAUR,TAUI
      REAL TR2,TI2,TR5,TI5
      REAL CR2,CI2,CR3,CI3,CR5,CI5,CR6,CI6
      REAL DR1,DI1,DR2,DI2,DR3,DI3,DR4,DI4,DR5,DI5,DR6,DI6
      REAL ER2,EI2,ER3,EI3,ER4,EI4,ER5,EI5,ER6,EI6
      DATA TAUR,TAUI /-.5,-.866025403784439/
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*6*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          TR2 = CCR(II,3)+CCR(II,5)
          CR3 = TAUI*(CCR(II,3)-CCR(II,5))
          CR2 = CCR(II,1)+TAUR*TR2
          DR1 = CCR(II,1)+TR2
          TI2 = CCI(II,3)+CCI(II,5)
          CI3 = TAUI*(CCI(II,3)-CCI(II,5))
          CI2 = CCI(II,1)+TAUR*TI2
          DI1 = CCI(II,1)+TI2
C          DR3 = CR2-CI3
C          DR5 = CR2+CI3
          TR5 = CCR(II,6)+CCR(II,2)
          CR6 = TAUI*(CCR(II,6)-CCR(II,2))
          CR5 = CCR(II,4)+TAUR*TR5
          DR2 = CCR(II,4)+TR5
          TI5 = CCI(II,6)+CCI(II,2)
          CI6 = TAUI*(CCI(II,6)-CCI(II,2))
          CI5 = CCI(II,4)+TAUR*TI5
          DI2 = CCI(II,4)+TI5
          CHI(IU,1) = DI1+DI2
          CHI(IU,4) = DI1-DI2
          CHR(IU,1) = DR1+DR2
          CHR(IU,4) = DR1-DR2
C          DR4 = CR5-CI6
C          DR6 = CR5+CI6
C          DI3 = CI2+CR3
C          DI5 = CI2-CR3
C          DI4 = CI5+CR6
C          DI6 = CI5-CR6
          CHI(IU,5) = CI2+CR3+(CI5+CR6)
          CHI(IU,2) = CI2+CR3-(CI5+CR6)
          CHI(IU,3) = CI2-CR3+(CI5-CR6)
          CHI(IU,6) = CI2-CR3-(CI5-CR6)
          CHR(IU,5) = CR2-CI3+(CR5-CI6)
          CHR(IU,2) = CR2-CI3-(CR5-CI6)
          CHR(IU,3) = CR2+CI3+(CR5+CI6)
          CHR(IU,6) = CR2+CI3-(CR5+CI6)
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*6*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          TR2 = CCR(II,3)+CCR(II,5)
          CR3 = TAUI*(CCR(II,3)-CCR(II,5))
          CR2 = CCR(II,1)+TAUR*TR2
          DR1 = CCR(II,1)+TR2
          TI2 = CCI(II,3)+CCI(II,5)
          CI3 = TAUI*(CCI(II,3)-CCI(II,5))
          CI2 = CCI(II,1)+TAUR*TI2
          DI1 = CCI(II,1)+TI2
          DR3 = CR2-CI3
          DR5 = CR2+CI3
          DI3 = CI2+CR3
          DI5 = CI2-CR3
          TR5 = CCR(II,6)+CCR(II,2)
          CR6 = TAUI*(CCR(II,6)-CCR(II,2))
          CR5 = CCR(II,4)+TAUR*TR5
          DR2 = CCR(II,4)+TR5
          TI5 = CCI(II,6)+CCI(II,2)
          CI6 = TAUI*(CCI(II,6)-CCI(II,2))
          CI5 = CCI(II,4)+TAUR*TI5
          DI2 = CCI(II,4)+TI5
          CHR(IU,1) = DR1+DR2
          ER4 = DR1-DR2
          CHI(IU,1) = DI1+DI2
          EI4 = DI1-DI2
          DR4 = CR5-CI6
          DR6 = CR5+CI6
          DI4 = CI5+CR6
          DI6 = CI5-CR6
          CHR(IU,4) = WA3(2*I-1)*ER4+WA3(2*I)*EI4
          CHI(IU,4) = WA3(2*I-1)*EI4-WA3(2*I)*ER4
          ER5 = DR3+DR4
          ER2 = DR3-DR4
          EI5 = DI3+DI4
          EI2 = DI3-DI4
          CHR(IU,2) = WA1(2*I-1)*ER2+WA1(2*I)*EI2
          CHI(IU,2) = WA1(2*I-1)*EI2-WA1(2*I)*ER2
          CHR(IU,5) = WA4(2*I-1)*ER5+WA4(2*I)*EI5
          CHI(IU,5) = WA4(2*I-1)*EI5-WA4(2*I)*ER5
          ER3 = DR5+DR6
          ER6 = DR5-DR6
          EI3 = DI5+DI6
          EI6 = DI5-DI6
          CHR(IU,3) = WA2(2*I-1)*ER3+WA2(2*I)*EI3
          CHI(IU,3) = WA2(2*I-1)*EI3-WA2(2*I)*ER3
          CHR(IU,6) = WA5(2*I-1)*ER6+WA5(2*I)*EI6
          CHI(IU,6) = WA5(2*I-1)*EI6-WA5(2*I)*ER6
  103   CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE PASSF8(CCR,CCI,CHR,CHI,INC,JMP,M,IDO,L1,
     & WA1,WA2,WA3,WA4,WA5,WA6,WA7)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,IDO,L1
      REAL CCR(INC*IDO,1),CCI(INC*IDO,1)
      REAL CHR(INC*IDO*L1,1),CHI(INC*IDO*L1,1)
      REAL WA1(2*IDO),WA2(2*IDO),WA3(2*IDO),WA4(2*IDO),WA5(2*IDO)
      REAL WA6(2*IDO),WA7(2*IDO)
      INTEGER I,J,K,II,IU
      REAL W
      REAL TR1,TI1,TR2,TI2,TR3,TI3,TR4,TI4
      REAL TR5,TI5,TR6,TI6,TR7,TI7,TR8,TI8
      REAL SR1,SI1,SR2,SI2,SR3,SI3,SR4,SI4
      REAL SR5,SI5,SR6,SI6,SR7,SI7,SR8,SI8
      REAL QR1,QI1,QR2,QI2,CR2,CI2,CR3,CI3,CR4,CI4
      REAL CR5,CI5,CR6,CI6,CR7,CI7,CR8,CI8
      DATA W/.7071067811865476/    
      IF (IDO .EQ. 1) THEN
        DO 101 K=1,L1
        DO 101 J=1,M
          II=1+JMP*(J-1)+INC*IDO*8*(K-1)
          IU=1+JMP*(J-1)+INC*IDO*(K-1)
          SR1 = CCR(II,1)+CCR(II,5)
          SR2 = CCR(II,1)-CCR(II,5)
          SR3 = CCR(II,2)+CCR(II,6)
          SR4 = CCR(II,2)-CCR(II,6)
          SR5 = CCR(II,3)+CCR(II,7)
          SR6 = CCR(II,3)-CCR(II,7)
          SR7 = CCR(II,4)+CCR(II,8)
          SR8 = CCR(II,4)-CCR(II,8)
          TR1 = SR1+SR5
          TR2 = SR1-SR5
          TR3 = SR3+SR7
          TR4 = SR3-SR7
          CHR(IU,1) = TR1+TR3 
          CHR(IU,5) = TR1-TR3
          SI1 = CCI(II,1)+CCI(II,5)
          SI2 = CCI(II,1)-CCI(II,5)
          SI3 = CCI(II,2)+CCI(II,6)
          SI4 = CCI(II,2)-CCI(II,6)
          SI5 = CCI(II,3)+CCI(II,7)
          SI6 = CCI(II,3)-CCI(II,7)
          SI7 = CCI(II,4)+CCI(II,8)
          SI8 = CCI(II,4)-CCI(II,8)
          TI1 = SI1+SI5
          TI2 = SI1-SI5
          TI3 = SI3+SI7
          TI4 = SI3-SI7
          CHI(IU,1) = TI1+TI3 
          CHI(IU,5) = TI1-TI3
          CHR(IU,3) = TR2+TI4
          CHR(IU,7) = TR2-TI4
          CHI(IU,3) = TI2-TR4
          CHI(IU,7) = TI2+TR4
          TR5 = SR2+SI6
          TR6 = SR2-SI6
          TR7 = W*(SR4+SI4)
          TI7 = W*(SI4-SR4)
          TR8 = W*(SR8+SI8)
          TI8 = W*(SI8-SR8)
          TI5 = SI2-SR6
          TI6 = SI2+SR6
          QI1 = TI7-TR8
          QR2 = TR8+TI7
          QR1 = TR7+TI8
          QI2 = TI8-TR7
          CHR(IU,2) = TR5+QR1
          CHR(IU,6) = TR5-QR1
          CHI(IU,2) = TI5+QI1
          CHI(IU,6) = TI5-QI1
          CHR(IU,4) = TR6+QR2
          CHR(IU,8) = TR6-QR2
          CHI(IU,4) = TI6+QI2
          CHI(IU,8) = TI6-QI2
  101   CONTINUE
      ELSE
        DO 103 K=1,L1
        DO 103 I=1,IDO
        DO 103 J=1,M
          II=1+JMP*(J-1)+INC*(IDO*8*(K-1)+(I-1))
          IU=1+JMP*(J-1)+INC*(IDO*(K-1)+(I-1))
          SR1 = CCR(II,1)+CCR(II,5)
          SR2 = CCR(II,1)-CCR(II,5)
          SR3 = CCR(II,2)+CCR(II,6)
          SR4 = CCR(II,2)-CCR(II,6)
          SR5 = CCR(II,3)+CCR(II,7)
          SR6 = CCR(II,3)-CCR(II,7)
          SR7 = CCR(II,4)+CCR(II,8)
          SR8 = CCR(II,4)-CCR(II,8)
          TR1 = SR1+SR5
          TR2 = SR1-SR5
          TR3 = SR3+SR7
          TR4 = SR3-SR7
          CHR(IU,1) = TR1+TR3 
          CR5 = TR1-TR3
          SI1 = CCI(II,1)+CCI(II,5)
          SI2 = CCI(II,1)-CCI(II,5)
          SI3 = CCI(II,2)+CCI(II,6)
          SI4 = CCI(II,2)-CCI(II,6)
          SI5 = CCI(II,3)+CCI(II,7)
          SI6 = CCI(II,3)-CCI(II,7)
          SI7 = CCI(II,4)+CCI(II,8)
          SI8 = CCI(II,4)-CCI(II,8)
          TI1 = SI1+SI5
          TI2 = SI1-SI5
          TI3 = SI3+SI7
          TI4 = SI3-SI7
          CHI(IU,1) = TI1+TI3 
          CI5 = TI1-TI3
          CHR(IU,5) = WA4(2*I-1)*CR5+WA4(2*I)*CI5
          CHI(IU,5) = WA4(2*I-1)*CI5-WA4(2*I)*CR5
          CR3 = TR2+TI4
          CR7 = TR2-TI4
          CI3 = TI2-TR4
          CI7 = TI2+TR4
          CHR(IU,3) = WA2(2*I-1)*CR3+WA2(2*I)*CI3
          CHI(IU,3) = WA2(2*I-1)*CI3-WA2(2*I)*CR3
          CHR(IU,7) = WA6(2*I-1)*CR7+WA6(2*I)*CI7
          CHI(IU,7) = WA6(2*I-1)*CI7-WA6(2*I)*CR7
          TR5 = SR2+SI6
          TR6 = SR2-SI6
          TR7 = W*(SR4+SI4)
          TI7 = W*(SI4-SR4)
          TR8 = W*(SR8+SI8)
          TI8 = W*(SI8-SR8)
          TI5 = SI2-SR6
          TI6 = SI2+SR6
          QI1 = TI7-TR8
          QR2 = TR8+TI7
          QR1 = TR7+TI8
          QI2 = TI8-TR7
          CR2 = TR5+QR1
          CR6 = TR5-QR1
          CI2 = TI5+QI1
          CI6 = TI5-QI1
          CHR(IU,2) = WA1(2*I-1)*CR2+WA1(2*I)*CI2
          CHI(IU,2) = WA1(2*I-1)*CI2-WA1(2*I)*CR2
          CHR(IU,6) = WA5(2*I-1)*CR6+WA5(2*I)*CI6
          CHI(IU,6) = WA5(2*I-1)*CI6-WA5(2*I)*CR6
          CR4 = TR6+QR2
          CR8 = TR6-QR2
          CI4 = TI6+QI2
          CI8 = TI6-QI2
          CHR(IU,4) = WA3(2*I-1)*CR4+WA3(2*I)*CI4
          CHI(IU,4) = WA3(2*I-1)*CI4-WA3(2*I)*CR4
          CHR(IU,8) = WA7(2*I-1)*CR8+WA7(2*I)*CI8
          CHI(IU,8) = WA7(2*I-1)*CI8-WA7(2*I)*CR8
  103   CONTINUE
      ENDIF
      RETURN
      END




      SUBROUTINE VCFFTI (N,WSAVE,IFAIL)
      DIMENSION       WSAVE(1)
      IFAILN=0
      IF (N .NE. 1) THEN
        IW1 = 1
        IW2 = IW1+N+N
        CALL CFFTI1 (N,WSAVE(IW1),WSAVE(IW2),IFAILN)
      ENDIF
      IF(IFAIL.EQ.0.AND.IFAILN.NE.0) THEN
        WRITE(*,*) 'VCFFTI-ERROR-CANNOT FACTOR',IFAILN
        STOP
      ENDIF
      IF(IFAIL.NE.0) IFAIL=IFAILN    
      RETURN
      END
      SUBROUTINE CFFTI1 (N,WA,IFAC,IFAIL)
      DIMENSION       WA(1),IFAC(1),NTRYH(6),NFAC(6)
      DATA NTRYH/2,3,4,5,6,8/
      IFAIL=0
      NL = N
      NF = 0
      NFAC(6)=0
C FIRST FACTOR N, SELECT LARGE FACTORS FIRST EXCEPT 8
      DO 101 J=5,1,-1
        NFAC(J)=0
        NTRY=NTRYH(J)
 102    IF (NL.EQ.NL/NTRY*NTRY) THEN
          NL=NL/NTRY
          NF=NF+1
          NFAC(J)=NFAC(J)+1
          GOTO 102
        ENDIF
 101  CONTINUE
      IF(NL.NE.1) THEN
        IFAIL=1
        RETURN
      ENDIF
C MAKE A SINGLE FACTOR 8 OUT OF A 4 AND A 2 IF NF BECOMES EVEN
C (SAVES THE OVERHEAD OF COPYING DATA AT LEAST)
      IF(NFAC(1).GE.1.AND.NFAC(3).GE.1.AND.NF/2*2.NE.NF) THEN
        NFAC(1)=NFAC(1)-1
        NFAC(3)=NFAC(3)-1
        NFAC(6)=NFAC(6)+1
        NF=NF-1
      ENDIF
C NOW INSERT THE FACTORS INTO IFAC BUT THE LARGEST FACTOR LAST
      NP=3
      DO 103 J=1,6
        DO 104 NP=NP,NP-1+NFAC(J)
          IFAC(NP)=NTRYH(J)
 104    CONTINUE
 103  CONTINUE
      IFAC(1) = N
      IFAC(2) = NF
C
      TPI = 6.28318530717959
      ARGH = TPI/FLOAT(N)
      I = 2
      L1 = 1
      DO 110 K1=1,NF
         IP = IFAC(K1+2)
         LD = 0
         L2 = L1*IP
         IDO = N/L2
         IDOT = IDO+IDO+2
         IPM = IP-1
         DO 109 J=1,IPM
            I1 = I
            WA(I-1) = 1.
            WA(I) = 0.
            LD = LD+L1
            FI=0.
            ARGLD = FLOAT(LD)*ARGH
            DO 108 II=4,IDOT,2
               I = I+2
               FI=FI+1.
               ARG = FI*ARGLD
               WA(I-1) = COS(ARG)
               WA(I) = SIN(ARG)
  108       CONTINUE
CC            IF (IP .LE. 5) GO TO 109
CC            WA(I1-1) = WA(I-1)
CC            WA(I1) = WA(I)
  109    CONTINUE
         L1 = L2
  110 CONTINUE
      RETURN
      END


      SUBROUTINE VCFFTS (CR,CI,WR,N,M,INC,JMP,WSAVE)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,N
      REAL CR(1+INC*(N+1)+JMP*(M-1)),CI(1+INC*(N+1)+JMP*(M-1))
      REAL WR(1+INC*(N+1)+1+JMP*(M-1))
      REAL WSAVE(15+2*N)
      CALL VCOST(CR,WR,N,M,INC,JMP,WSAVE)
      CALL VCOST(CI,WR,N,M,INC,JMP,WSAVE)
      RETURN
      END

      SUBROUTINE VCFTAB (CR,CI,WR,WI,N,M,INC,JMP,WSAVE)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,N,IW1,IW2,NP1
      REAL CR(1+INC*N+JMP*(M-1)),CI(1+INC*N+JMP*(M-1))
      REAL WR(1+INC*N+JMP*(M-1)),WI(1+INC*N+JMP*(M-1))
      REAL WSAVE(15+2*N)
      NP1 = N+1
      IW1 = N/2+1
      IW2 = IW1+NP1
      CALL CFTAB1(CR,CI,WR,WI,N,M,INC,JMP,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END

      SUBROUTINE CFTAB1(XR,XI,WR,WI,N,M,INC,JMP,WAS,WAR,IFAC)
      DIMENSION WAR(1),WAS(1),IFAC(1)
      REAL XR(INC,1),XI(INC,1),WR(INC,1),WI(INC,1)
      DATA SQRT3 /1.73205080756888/
      IF(N.EQ.1) THEN
        DO 200 J=1,M
          JJ=1+(J-1)*JMP
          T1 = 2.*XR(JJ,1)
          XR(JJ,1) = -2.*XI(JJ,1)
          XI(JJ,1) = T1
 200    CONTINUE
      ELSE
      NP1 = N+1
      NS2 = N/2
      DO 205 J=1,M
        JJ=1+(J-1)*JMP
        WR(JJ,1) = 0.
        WI(JJ,1) = 0.
 205  CONTINUE
      DO 104 K=1,NS2
        KC = NP1-K
CDIR$ IVDEP
        DO 202 J=1,M
          JJ=1+(J-1)*JMP
          T1 = XR(JJ,K)-XR(JJ,KC)
          T2 = WAS(K)*(XR(JJ,K)+XR(JJ,KC))
	  WR(JJ,K+1) = T1+T2
	  WR(JJ,KC+1) = T2-T1
          T1 = XI(JJ,K)-XI(JJ,KC)
          T2 = WAS(K)*(XI(JJ,K)+XI(JJ,KC))
	  WI(JJ,K+1) = T1+T2
	  WI(JJ,KC+1) = T2-T1
  202   CONTINUE
  104 CONTINUE
      DO 203 J=1,M
        JJ=1+(J-1)*JMP
        WR(JJ,NS2+2) = 4.*XR(JJ,NS2+1)
        WI(JJ,NS2+2) = 4.*XI(JJ,NS2+1)
 203  CONTINUE
      CALL RFFTF1 (WR,WR(1,2),XR,XR(1,2),NP1,M,2*INC,JMP,WAR,IFAC)
      CALL RFFTF1 (WI,WI(1,2),XI,XI(1,2),NP1,M,2*INC,JMP,WAR,IFAC)
CDIR$ IVDEP
      DO 204 J=1,M
        JJ=1+(J-1)*JMP
        XI(JJ,1) = .5*WR(JJ,1)
        XR(JJ,1) = -.5*WI(JJ,1)
        XI(JJ,N+1) = 0.
        XR(JJ,N+1) = 0.
 204  CONTINUE
      DO 105 I=3,N,2
CDIR$ IVDEP
      DO 105 J=1,M
         JJ=1+(J-1)*JMP
	 XI(JJ,I-1) = -WR(JJ,I+1)
	 XI(JJ,I) = XI(JJ,I-2)+WR(JJ,I)
	 XR(JJ,I-1) = WI(JJ,I+1)
	 XR(JJ,I) = XR(JJ,I-2)-WI(JJ,I)
  105 CONTINUE
      RETURN
      ENDIF
      END
      SUBROUTINE VCFTAF (CR,CI,WR,WI,N,M,INC,JMP,WSAVE)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,N,IW1,IW2,NP1
      REAL CR(1+INC*N+JMP*(M-1)),CI(1+INC*N+JMP*(M-1))
      REAL WR(1+INC*N+JMP*(M-1)),WI(1+INC*N+JMP*(M-1))
      REAL WSAVE(15+2*N)
      NP1 = N+1
      IW1 = N/2+1
      IW2 = IW1+NP1
      CALL CFTAF1(CR,CI,WR,WI,N,M,INC,JMP,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END

      SUBROUTINE CFTAF1(XR,XI,WR,WI,N,M,INC,JMP,WAS,WAR,IFAC)
      DIMENSION WAR(1),WAS(1),IFAC(1)
      REAL XR(INC,1),XI(INC,1),WR(INC,1),WI(INC,1)
      DATA SQRT3 /1.73205080756888/
      IF(N.EQ.1) THEN
        DO 200 J=1,M
          JJ=1+(J-1)*JMP
          T1 = -2.*XR(JJ,1)
          XR(JJ,1) = 2.*XI(JJ,1)
          XI(JJ,1) = T1
 200    CONTINUE
      ELSE
      NP1 = N+1
      NS2 = N/2
      DO 205 J=1,M
        JJ=1+(J-1)*JMP
        WR(JJ,1) = 0.
        WI(JJ,1) = 0.
 205  CONTINUE
      DO 104 K=1,NS2
        KC = NP1-K
CDIR$ IVDEP
        DO 202 J=1,M
          JJ=1+(J-1)*JMP
          T1 = XR(JJ,K)-XR(JJ,KC)
          T2 = WAS(K)*(XR(JJ,K)+XR(JJ,KC))
	  WR(JJ,K+1) = T1+T2
	  WR(JJ,KC+1) = T2-T1
          T1 = XI(JJ,K)-XI(JJ,KC)
          T2 = WAS(K)*(XI(JJ,K)+XI(JJ,KC))
	  WI(JJ,K+1) = T1+T2
	  WI(JJ,KC+1) = T2-T1
  202   CONTINUE
  104 CONTINUE
      DO 203 J=1,M
        JJ=1+(J-1)*JMP
        WR(JJ,NS2+2) = 4.*XR(JJ,NS2+1)
        WI(JJ,NS2+2) = 4.*XI(JJ,NS2+1)
 203  CONTINUE
      CALL RFFTF1 (WR,WR(1,2),XR,XR(1,2),NP1,M,2*INC,JMP,WAR,IFAC)
      CALL RFFTF1 (WI,WI(1,2),XI,XI(1,2),NP1,M,2*INC,JMP,WAR,IFAC)
CDIR$ IVDEP
      DO 204 J=1,M
        JJ=1+(J-1)*JMP
        XI(JJ,1) = -.5*WR(JJ,1)
        XR(JJ,1) = .5*WI(JJ,1)
        XI(JJ,N+1) = 0.
        XR(JJ,N+1) = 0.
 204  CONTINUE
      DO 105 I=3,N,2
CDIR$ IVDEP
      DO 105 J=1,M
         JJ=1+(J-1)*JMP
	 XI(JJ,I-1) = WR(JJ,I+1)
	 XI(JJ,I) = XI(JJ,I-2)-WR(JJ,I)
	 XR(JJ,I-1) = -WI(JJ,I+1)
	 XR(JJ,I) = XR(JJ,I-2)+WI(JJ,I)
  105 CONTINUE
      RETURN
      ENDIF
      END
      SUBROUTINE VCHBB (X,W,N,M,INC,JMP,WSAVE)
      REAL WSAVE(2*N+15),X(INC,1),W(INC,1)
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      IF (N.GT.1) THEN
        IF(N.EQ.3) THEN
          DO 101 J=1,M
            JJ=1+(J-1)*JMP
            X1P3 = X(JJ,1)+X(JJ,3)
            TX2 = X(JJ,2)
            X(JJ,2) = (X(JJ,1)-X(JJ,3))
            X(JJ,1) = (X1P3+TX2)
            X(JJ,3) = (X1P3-TX2)
 101        CONTINUE
        ELSE
CDIR$ IVDEP
          DO 102 J=1,M
            JJ=1+(J-1)*JMP
            W(JJ,1) = X(JJ,1)-X(JJ,N)
            X(JJ,1) = X(JJ,1)+X(JJ,N)
 102      CONTINUE
          DO 103 K=2,NS2
            KC = NP1-K
CDIR$ IVDEP
            DO 103 J=1,M
              JJ=1+(J-1)*JMP
              T1 = .5*(X(JJ,K)+X(JJ,KC))
              T2 = X(JJ,K)-X(JJ,KC)
              W(JJ,1) = W(JJ,1)+.5*WSAVE(KC)*T2
              T2 = .5*WSAVE(K)*T2
              X(JJ,K) = T1-T2
              X(JJ,KC) = T1+T2
 103      CONTINUE
          DO 104 J=1,M
            JJ=1+(J-1)*JMP
C            X(JJ,NS2+1) = 2.*X(JJ,NS2+1)
 104      CONTINUE
          CALL RFFTF1 (X,X(1,2),W,W(1,2),NM1,M,
     &      2*INC,JMP,WSAVE(N+1),WSAVE(N+N))
          DO 105 J=1,M
            JJ=1+(J-1)*JMP
            X(JJ,2) = W(JJ,1)
 105      CONTINUE
          DO 106 I=4,N,2
CDIR$ IVDEP
          DO 106 J=1,M
            JJ=1+(J-1)*JMP
            X(JJ,I) = X(JJ,I-2)-X(JJ,I)
 106      CONTINUE
        ENDIF
      ENDIF
      RETURN
      END

      SUBROUTINE VCHBF (X,W,N,M,INC,JMP,WSAVE)
      REAL WSAVE(2*N+15),X(INC,1),W(INC,1)
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      IF (N.GT.1) THEN
        IF(N.EQ.3) THEN
          DO 101 J=1,M
            JJ=1+(J-1)*JMP
            X1P3 = X(JJ,1)+X(JJ,3)
            TX2 = X(JJ,2)+X(JJ,2)
            X(JJ,2) = (X(JJ,1)-X(JJ,3))*.5
            X(JJ,1) = (X1P3+TX2)*.25
            X(JJ,3) = (X1P3-TX2)*.25
 101        CONTINUE
        ELSE
CDIR$ IVDEP
          DO 102 J=1,M
            JJ=1+(J-1)*JMP
            W(JJ,1) = (X(JJ,1)-X(JJ,N))*.5
            X(JJ,1) = (X(JJ,1)+X(JJ,N))*.5
 102      CONTINUE
          DO 103 K=2,NS2
            KC = NP1-K
CDIR$ IVDEP
            DO 103 J=1,M
              JJ=1+(J-1)*JMP
              T1 = .5*(X(JJ,K)+X(JJ,KC))
              T2 = X(JJ,K)-X(JJ,KC)
              W(JJ,1) = W(JJ,1)+.5*WSAVE(KC)*T2
              T2 = .5*WSAVE(K)*T2
              X(JJ,K) = T1-T2
              X(JJ,KC) = T1+T2
 103      CONTINUE
          CALL RFFTF1 (X,X(1,2),W,W(1,2),NM1,M,
     &      2*INC,JMP,WSAVE(N+1),WSAVE(N+N))
          DO 105 J=1,M
            JJ=1+(J-1)*JMP
            X(JJ,2) = W(JJ,1)
 105      CONTINUE
          DO 106 I=4,N,2
          DO 106 J=1,M
            JJ=1+(J-1)*JMP
            X(JJ,I) = X(JJ,I-2)-X(JJ,I)
 106      CONTINUE
CDIR$ IVDEP
          DO 107 J=1,M
            JJ=1+(J-1)*JMP
            X(JJ,1) = X(JJ,1)*.5
            X(JJ,N) = X(JJ,N)*.5
 107     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END


      SUBROUTINE VCOST (X,W,N,M,INC,JMP,WSAVE)
      REAL WSAVE(2*N+15),X(INC,1),W(INC,1)
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      IF (N.GT.1) THEN
        IF(N.EQ.3) THEN
          DO 101 J=1,M
            JJ=1+(J-1)*JMP
            X1P3 = X(JJ,1)+X(JJ,3)
            TX2 = X(JJ,2)+X(JJ,2)
            X(JJ,2) = X(JJ,1)-X(JJ,3)
            X(JJ,1) = X1P3+TX2
            X(JJ,3) = X1P3-TX2
 101        CONTINUE
        ELSE
CDIR$ IVDEP
          DO 102 J=1,M
            JJ=1+(J-1)*JMP
            W(JJ,1) = X(JJ,1)-X(JJ,N)
            X(JJ,1) = X(JJ,1)+X(JJ,N)
 102      CONTINUE
          DO 103 K=2,NS2
            KC = NP1-K
CDIR$ IVDEP
            DO 103 J=1,M
              JJ=1+(J-1)*JMP
              T1 = X(JJ,K)+X(JJ,KC)
              T2 = X(JJ,K)-X(JJ,KC)
              W(JJ,1) = W(JJ,1)+WSAVE(KC)*T2
              X(JJ,K) = T1-WSAVE(K)*T2
              X(JJ,KC) = T1+WSAVE(K)*T2
 103      CONTINUE
          DO 104 J=1,M
            JJ=1+(J-1)*JMP
            X(JJ,NS2+1) = 2.*X(JJ,NS2+1)
 104      CONTINUE
          CALL RFFTF1 (X,X(1,2),W,W(1,2),NM1,M,
     &      2*INC,JMP,WSAVE(N+1),WSAVE(N+N))
          DO 105 J=1,M
            JJ=1+(J-1)*JMP
C            W(JJ,2) = X(JJ,3)
            X(JJ,2) = W(JJ,1)
 105      CONTINUE
          DO 106 I=4,N,2
CDIR$ IVDEP
          DO 106 J=1,M
            JJ=1+(J-1)*JMP
            X(JJ,I) = X(JJ,I-2)-X(JJ,I)
 106      CONTINUE
          DO 107 J=1,M
            JJ=1+(J-1)*JMP
 107     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END

      SUBROUTINE VCOSTI (N,WSAVE,IFAIL)
      DIMENSION       WSAVE(2*N+15)
      DATA PI /3.14159265358979/
      IFAILN=0
      IF (N .EQ. 2) IFAILN=1
      IF (N .GT. 3) THEN
        NM1 = N-1
        NP1 = N+1
        NS2 = N/2
        DT = PI/FLOAT(NM1)
        FK = 0.
        DO 101 K=2,NS2
          KC = NP1-K
          FK = FK+1.
          WSAVE(K) = 2.*SIN(FK*DT)
          WSAVE(KC) = 2.*COS(FK*DT)
  101   CONTINUE
        CALL RFFTI1 (NM1,WSAVE(N+1),WSAVE(N+N),IFAILN)
      ENDIF
      IF(IFAIL.EQ.0.AND.IFAILN.NE.0) THEN
          IF(IFAILN.EQ.1) THEN
            WRITE(*,*) 'VCOSTI-ERROR-N MUST BE ODD'
          ELSE
            WRITE(*,*) 'VCOSTI-ERROR-CANNOT FACTOR',IFAILN
          ENDIF
        STOP
      ENDIF
      IF(IFAIL.NE.0) IFAIL=IFAILN    
      RETURN
      END

      SUBROUTINE VRFFTB (CR,CI,WR,WI,N,M,INC,JMP,WSAVE)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,N
      REAL CR(1+INC*(N/2)+JMP*(M-1)),CI(1+INC*(N/2)+JMP*(M-1))
      REAL WR(1+INC*(N/2)+JMP*(M-1)),WI(1+INC*(N/2)+JMP*(M-1))
      REAL WSAVE(N+15)
      IF (N .EQ. 1) RETURN
      CALL RFFTB1 (CR,CI,WR,WI,N,M,INC,JMP,WSAVE,WSAVE(N+1))
      RETURN
      END
      SUBROUTINE RFFTB1 (CR,CI,WR,WI,N,M,INC,JMP,WA,IFAC)
      REAL CR(1+INC*(N/2)+JMP*(M-1)),CI(1+INC*(N/2)+JMP*(M-1))
      REAL WR(1+INC*(N/2)+JMP*(M-1)),WI(1+INC*(N/2)+JMP*(M-1))
      INTEGER IFAC(15)
      REAL WA(N)
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 100 J=1,M
        JJ=1+JMP*(J-1)
        CI(JJ)=CR(JJ)
 100  CONTINUE
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDL1 = IDO*L1
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF(IDO.EQ.1) THEN
           IF(NA.EQ.0) THEN
             IF(IP.EQ.4)CALL RADB4L (L1,INC,JMP,M,CR,CI,WR,WI)
             IF(IP.EQ.2)CALL RADB2L (L1,INC,JMP,M,CR,CI,WR,WI)
           ELSE      
             IF(IP.EQ.4)CALL RADB4L (L1,INC,JMP,M,WR,WI,CR,CI)
             IF(IP.EQ.2)CALL RADB2L (L1,INC,JMP,M,WR,WI,CR,CI)
           ENDIF
         ELSE
           IF(NA .EQ. 0) THEN
             IF(IP.EQ.4)CALL RADB4 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                   WA(IW),WA(IX2),WA(IX3))
             IF(IP.EQ.2)CALL RADB2 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                   WA(IW))
             IF(IP.EQ.3)CALL RADB3 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                   WA(IW),WA(IX2))
             IF(IP.EQ.5)CALL RADB5 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                WA(IW),WA(IX2),WA(IX3),WA(IX4))
           ELSE
             IF(IP.EQ.4)CALL RADB4 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &                   WA(IW),WA(IX2),WA(IX3))
             IF(IP.EQ.2)CALL RADB2 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &                   WA(IW))
             IF(IP.EQ.3)CALL RADB3 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &          WA(IW),WA(IX2))
             IF(IP.EQ.5)CALL RADB5 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &          WA(IW),WA(IX2),WA(IX3),WA(IX4))
           ENDIF
         ENDIF
         IF(IP.GT.5) THEN
           WRITE(*,*) 'FACTOR ',IP,' NOT IMPLEMENTED'
           STOP
         ENDIF
         NA=1-NA
         L1 = L2
         IW = IW+(IP-1)*IDO
  116 CONTINUE
      IF (NA .NE. 0) THEN
        DO 117 I=1,N/2
        DO 117 J=1,M
          JJ=1+JMP*(J-1)
          CR(JJ+INC*(I-1)) = WR(JJ+INC*(I-1))
          CI(JJ+INC*(I-1)) = WI(JJ+INC*(I-1))
  117   CONTINUE
      ENDIF
      DO 118 J=1,M
        JJ=1+JMP*(J-1)
        CR(JJ+INC*(N/2)) = 0.0
        CI(JJ+INC*(N/2)) = 0.0
 118  CONTINUE
      RETURN
      END

      SUBROUTINE RADB2L (L1,INC,JMP,M,CCR,CCI,CHR,CHI)
      REAL CCR(INC,L1+1),CCI(INC,L1),CHR(INC,L1+1),CHI(INC,L1)
      IF(MOD(L1,2).EQ.0) THEN
        DO 101 K=1,L1/2
        DO 101 J=1,M
          JJ=1+JMP*(J-1)
          CHR(JJ,K) = CCI(JJ,2*K-1)+CCR(JJ,2*K)
          CHR(JJ,K+L1/2) = CCI(JJ,2*K-1)-CCR(JJ,2*K)
          CHI(JJ,K) = CCI(JJ,2*K)+CCR(JJ,2*K+1)
          CHI(JJ,K+L1/2) = CCI(JJ,2*K)-CCR(JJ,2*K+1)
 101    CONTINUE
      ELSE
        DO 102 K=1,L1/2
        DO 102 J=1,M
          JJ=1+JMP*(J-1)
          CHR(JJ,K) = CCI(JJ,2*K-1)+CCR(JJ,2*K)
          CHI(JJ,K+L1/2) = CCI(JJ,2*K-1)-CCR(JJ,2*K)
          CHI(JJ,K) = CCI(JJ,2*K)+CCR(JJ,2*K+1)
          CHR(JJ,K+L1/2+1) = CCI(JJ,2*K)-CCR(JJ,2*K+1)
 102      CONTINUE
        DO 103 J=1,M
          JJ=1+JMP*(J-1)
          CHR(JJ,L1/2+1) = CCI(JJ,L1)+CCR(JJ,L1+1)
          CHI(JJ,L1) = CCI(JJ,L1)-CCR(JJ,L1+1)
 103    CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE RADB2 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,WA1)
      REAL CCR(INC,IDO/2,2,L1),CCI(INC,IDO/2,2,L1)
      REAL CHR(INC,IDO/2,L1,2),CHI(INC,IDO/2,L1,2)
      REAL WA1(1)
      DO 101 K=1,L1
      DO 101 J=1,M
        JJ=1+JMP*(J-1)
        CHI(JJ,1,K,1) = CCI(JJ,1,1,K)+CCR(JJ,IDO/2+1,2,K)
        CHI(JJ,1,K,2) = CCI(JJ,1,1,K)-CCR(JJ,IDO/2+1,2,K)
  101 CONTINUE
      IF (IDO.GT.2) THEN
        IDP2 = IDO/2+2
        DO 103 K=1,L1
        DO 103 I=2,IDO/2
          IC = IDP2-I
          DO 104 J=1,M
            JJ=1+JMP*(J-1)
            CHR(JJ,I,K,1) = CCR(JJ,I,1,K)+CCR(JJ,IC,2,K)
            TR2 = CCR(JJ,I,1,K)-CCR(JJ,IC,2,K)
            CHI(JJ,I,K,1) = CCI(JJ,I,1,K)-CCI(JJ,IC,2,K)
            TI2 = CCI(JJ,I,1,K)+CCI(JJ,IC,2,K)
            CHR(JJ,I,K,2) = WA1(2*I-3)*TR2-WA1(2*I-2)*TI2
            CHI(JJ,I,K,2) = WA1(2*I-3)*TI2+WA1(2*I-2)*TR2
  104     CONTINUE
  103   CONTINUE
      ENDIF
      DO 106 K=1,L1
      DO 106 J=1,M
        JJ=1+JMP*(J-1)
        CHR(JJ,IDO/2+1,K,1) = CCR(JJ,IDO/2+1,1,K)+CCR(JJ,IDO/2+1,1,K)
        CHR(JJ,IDO/2+1,K,2) = -(CCI(JJ,1,2,K)+CCI(JJ,1,2,K))
  106 CONTINUE
      RETURN
      END

      SUBROUTINE RADB3 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,WA1,WA2)
      REAL CCR(INC,IDO/2,3,L1),CCI(INC,IDO/2,3,L1)
      REAL CHR(INC,IDO/2,L1,3),CHI(INC,IDO/2,L1,3)
      REAL  WA1(1)     ,WA2(1)
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
      DO 101 J=1,M
         JJ=1+JMP*(J-1)
         TR2 = CCR(JJ,IDO/2+1,2,K)+CCR(JJ,IDO/2+1,2,K)
         CR2 = CCI(JJ,1,1,K)+TAUR*TR2
         CHI(JJ,1,K,1) = CCI(JJ,1,1,K)+TR2
         CI3 = TAUI*(CCI(JJ,1,3,K)+CCI(JJ,1,3,K))
         CHI(JJ,1,K,2) = CR2-CI3
         CHI(JJ,1,K,3) = CR2+CI3
  101 CONTINUE
      IDP2 = IDO/2+2
      DO 103 K=1,L1
      DO 103 I=2,IDO/2
        IC = IDP2-I
        DO 104 J=1,M
          JJ=1+JMP*(J-1)
          TR2 = CCR(JJ,I,3,K)+CCR(JJ,IC,2,K)
          CR3 = TAUI*(CCR(JJ,I,3,K)-CCR(JJ,IC,2,K))
          CR2 = CCR(JJ,I,1,K)+TAUR*TR2
          CHR(JJ,I,K,1) = CCR(JJ,I,1,K)+TR2
          TI2 = CCI(JJ,I,3,K)-CCI(JJ,IC,2,K)
          CI3 = TAUI*(CCI(JJ,I,3,K)+CCI(JJ,IC,2,K))
          CI2 = CCI(JJ,I,1,K)+TAUR*TI2
          CHI(JJ,I,K,1) = CCI(JJ,I,1,K)+TI2
          DR2 = CR2-CI3
          DI2 = CI2+CR3
          CHR(JJ,I,K,2) = WA1(2*I-3)*DR2-WA1(2*I-2)*DI2
          CHI(JJ,I,K,2) = WA1(2*I-3)*DI2+WA1(2*I-2)*DR2
C          DI3 = CI2-CR3
C          DR3 = CR2+CI3
          CHR(JJ,I,K,3) = WA2(2*I-3)*(CR2+CI3)-WA2(2*I-2)*(CI2-CR3)
          CHI(JJ,I,K,3) = WA2(2*I-3)*(CI2-CR3)+WA2(2*I-2)*(CR2+CI3)
  104   CONTINUE
  103 CONTINUE
      DO 106 K=1,L1
      DO 106 J=1,M
        JJ=1+JMP*(J-1)
        TR1 = CCR(JJ,IDO/2+1,1,K)-CCR(JJ,IDO/2+1,3,K)
        CHR(JJ,IDO/2+1,K,1) = 2.*CCR(JJ,IDO/2+1,1,K)+CCR(JJ,IDO/2+1,3,K)
        CHR(JJ,IDO/2+1,K,2) = TR1-(2.*TAUI)*CCI(JJ,1,2,K)
        CHR(JJ,IDO/2+1,K,3) = -TR1-(2.*TAUI)*CCI(JJ,1,2,K)
  106 CONTINUE
      RETURN
      END

      SUBROUTINE RADB4L (L1,INC,JMP,M,CCR,CCI,CHR,CHI)
      REAL CCR(INC,2,L1+1),CCI(INC,2,L1)
      REAL CHR(INC,(L1+1)/2,4),CHI(INC,(L1+1)/2,4)
      IF(MOD(L1,2).EQ.0) THEN
        DO 101 K=1,L1/2
        DO 101 J=1,M
           JJ=1+JMP*(J-1)
C          TR1 = CCI(JJ,1,2*K-1)-CCR(JJ,1,2*K)
C          TR2 = CCI(JJ,1,2*K-1)+CCR(JJ,1,2*K)
C          TR3 = 2.*CCR(JJ,2,2*K-1)
C          TR4 = 2.*CCI(JJ,2,2*K-1)
          CHR(JJ,K,4) = CCI(JJ,1,2*K-1)-CCR(JJ,1,2*K)+2.*CCI(JJ,2,2*K-1)
          CHR(JJ,K,2) = CCI(JJ,1,2*K-1)-CCR(JJ,1,2*K)-2.*CCI(JJ,2,2*K-1)
          CHR(JJ,K,1) = CCI(JJ,1,2*K-1)+CCR(JJ,1,2*K)+2.*CCR(JJ,2,2*K-1)
          CHR(JJ,K,3) = CCI(JJ,1,2*K-1)+CCR(JJ,1,2*K)-2.*CCR(JJ,2,2*K-1)
C          TI1 = CCI(JJ,1,2*K)-CCR(JJ,1,2*K+1)
C          TI2 = CCI(JJ,1,2*K)+CCR(JJ,1,2*K+1)
C          TI3 = 2.*CCR(JJ,2,2*K)
C          TI4 = 2.*CCI(JJ,2,2*K)
          CHI(JJ,K,4) = CCI(JJ,1,2*K)-CCR(JJ,1,2*K+1)+2.*CCI(JJ,2,2*K)
          CHI(JJ,K,2) = CCI(JJ,1,2*K)-CCR(JJ,1,2*K+1)-2.*CCI(JJ,2,2*K)
          CHI(JJ,K,1) = CCI(JJ,1,2*K)+CCR(JJ,1,2*K+1)+2.*CCR(JJ,2,2*K)
          CHI(JJ,K,3) = CCI(JJ,1,2*K)+CCR(JJ,1,2*K+1)-2.*CCR(JJ,2,2*K)
 101    CONTINUE
      ELSE
        DO 102 K=1,L1/2
        DO 102 J=1,M
          JJ=1+JMP*(J-1)
          TR1 = CCI(JJ,1,2*K-1)-CCR(JJ,1,2*K)
          TR2 = CCI(JJ,1,2*K-1)+CCR(JJ,1,2*K)
          TR3 = CCR(JJ,2,2*K-1)+CCR(JJ,2,2*K-1)
          TR4 = CCI(JJ,2,2*K-1)+CCI(JJ,2,2*K-1)
          CHR(JJ,K,1) = TR2+TR3
          CHR(JJ,K-1,3) = TR2-TR3
          CHI(JJ,K-2,4) = TR1+TR4
          CHI(JJ,K-1,2) = TR1-TR4
          TR1 = CCI(JJ,1,2*K)-CCR(JJ,1,2*K+1)
          TR2 = CCI(JJ,1,2*K)+CCR(JJ,1,2*K+1)
          TR3 = CCR(JJ,2,2*K)+CCR(JJ,2,2*K)
          TR4 = CCI(JJ,2,2*K)+CCI(JJ,2,2*K)
          CHI(JJ,K,1) = TR2+TR3
          CHI(JJ,K-1,3) = TR2-TR3
          CHR(JJ,K-1,4) = TR1+TR4
          CHR(JJ,K,2) = TR1-TR4
 102    CONTINUE
        DO 103 J=1,M
          JJ=1+JMP*(J-1)
          TR1 = CCI(JJ,1,L1)-CCR(JJ,1,L1+1)
          TR2 = CCI(JJ,1,L1)+CCR(JJ,1,L1+1)
          TR3 = 2.*CCR(JJ,2,L1)
          TR4 = 2.*CCI(JJ,2,L1)
          CHR(JJ,L1/2+1,1) = TR2+TR3
          CHR(JJ,L1/2,3) = TR2-TR3
          CHI(JJ,L1/2-1,4) = TR1+TR4
          CHI(JJ,L1/2,2) = TR1-TR4
 103    CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE RADB4 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,WA1,WA2,WA3)
      REAL CCR(INC,IDO/2,4,L1),CCI(INC,IDO/2,4,L1)
      REAL CHR(INC,IDO/2,L1,4),CHI(INC,IDO/2,L1,4)
      REAL WA1(1)     ,WA2(1)     ,WA3(1)
      DATA SQRT2 /1.414213562373095/
      DO 101 K=1,L1
      DO 101 J=1,M
        JJ=1+JMP*(J-1)
        TR1 = CCI(JJ,1,1,K)-CCR(JJ,IDO/2+1,4,K)
        TR2 = CCI(JJ,1,1,K)+CCR(JJ,IDO/2+1,4,K)
C        TR3 = 2.*CCR(JJ,IDO/2+1,2,K)
C        TR4 = 2.*CCI(JJ,1,3,K)
        CHI(JJ,1,K,4) = TR1+2.*CCI(JJ,1,3,K)
        CHI(JJ,1,K,2) = TR1-2.*CCI(JJ,1,3,K)
        CHI(JJ,1,K,1) = TR2+2.*CCR(JJ,IDO/2+1,2,K)
        CHI(JJ,1,K,3) = TR2-2.*CCR(JJ,IDO/2+1,2,K)
  101 CONTINUE
      IF (IDO.GT.2) THEN
        IDP2 = IDO/2+2
        DO 104 K=1,L1
        DO 104 I=2,IDO/2
          IC = IDP2-I
          DO 103 J=1,M
            JJ=1+JMP*(J-1)
            TR3 = CCR(JJ,I,3,K)+CCR(JJ,IC,2,K)
            TI4 = CCR(JJ,I,3,K)-CCR(JJ,IC,2,K)
            TR2 = CCR(JJ,I,1,K)+CCR(JJ,IC,4,K)
            CR3 = TR2-TR3
            TR1 = CCR(JJ,I,1,K)-CCR(JJ,IC,4,K)
            CHR(JJ,I,K,1) = TR2+TR3
            TI1 = CCI(JJ,I,1,K)+CCI(JJ,IC,4,K)
            TI2 = CCI(JJ,I,1,K)-CCI(JJ,IC,4,K)
            TI3 = CCI(JJ,I,3,K)-CCI(JJ,IC,2,K)
            CHI(JJ,I,K,1) = TI2+TI3
            TR4 = CCI(JJ,I,3,K)+CCI(JJ,IC,2,K)
C            CI3 = TI2-TI3
            CHR(JJ,I,K,3) = WA2(2*I-3)*CR3-WA2(2*I-2)*(TI2-TI3)
            CHI(JJ,I,K,3) = WA2(2*I-3)*(TI2-TI3)+WA2(2*I-2)*CR3
C            CR2 = TR1-TR4
C            CR4 = TR1+TR4
C            CI2 = TI1+TI4
C            CI4 = TI1-TI4
            CHR(JJ,I,K,2) = WA1(2*I-3)*(TR1-TR4)-WA1(2*I-2)*(TI1+TI4)
            CHI(JJ,I,K,2) = WA1(2*I-3)*(TI1+TI4)+WA1(2*I-2)*(TR1-TR4)
            CHR(JJ,I,K,4) = WA3(2*I-3)*(TR1+TR4)-WA3(2*I-2)*(TI1-TI4)
            CHI(JJ,I,K,4) = WA3(2*I-3)*(TI1-TI4)+WA3(2*I-2)*(TR1+TR4)
  103     CONTINUE
  104   CONTINUE
      ENDIF
      DO 106 K=1,L1
      DO 106 J=1,M
         JJ=1+JMP*(J-1)
         TI1 = CCI(JJ,1,2,K)+CCI(JJ,1,4,K)
C         TI2 = CCI(JJ,1,4,K)-CCI(JJ,1,2,K)
         CHR(JJ,IDO/2+1,K,3) = 2.*(CCI(JJ,1,4,K)-CCI(JJ,1,2,K))
         TR1 = CCR(JJ,IDO/2+1,1,K)-CCR(JJ,IDO/2+1,3,K)
C         TR2 = CCR(JJ,IDO/2+1,1,K)+CCR(JJ,IDO/2+1,3,K)
         CHR(JJ,IDO/2+1,K,1) = 
     &        2.*(CCR(JJ,IDO/2+1,1,K)+CCR(JJ,IDO/2+1,3,K))
         CHR(JJ,IDO/2+1,K,2) = SQRT2*(TR1-TI1)
         CHR(JJ,IDO/2+1,K,4) = -SQRT2*(TR1+TI1)
  106 CONTINUE
      RETURN
      END
      SUBROUTINE RADB5 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,
     &WA1,WA2,WA3,WA4)
      REAL CCR(INC,IDO/2,5,L1),CCI(INC,IDO/2,5,L1)
      REAL CHR(INC,IDO/2,L1,5),CHI(INC,IDO/2,L1,5)
      REAL WA1(1)     ,WA2(1)     ,WA3(1)     ,WA4(1)
      DATA TR11,TI11,TR12,TI12,TR13 /.309016994374947,.951056516295155,
     1-.809016994374947,.587785252292473,.55901699437495/
      DO 101 K=1,L1
      DO 101 J=1,M
         JJ=1+JMP*(J-1)
         TI5 = CCI(JJ,1,3,K)+CCI(JJ,1,3,K)
         TI4 = CCI(JJ,1,5,K)+CCI(JJ,1,5,K)
         TR2 = CCR(JJ,IDO/2+1,2,K)+CCR(JJ,IDO/2+1,2,K)
         TR3 = CCR(JJ,IDO/2+1,4,K)+CCR(JJ,IDO/2+1,4,K)
         CHI(JJ,1,K,1) = CCI(JJ,1,1,K)+TR2+TR3
         CR2 = CCI(JJ,1,1,K)+TR11*TR2+TR12*TR3
         CR3 = CCI(JJ,1,1,K)+TR12*TR2+TR11*TR3
         CI5 = TI11*TI5+TI12*TI4
         CI4 = TI12*TI5-TI11*TI4
         CHI(JJ,1,K,2) = CR2-CI5
         CHI(JJ,1,K,3) = CR3-CI4
         CHI(JJ,1,K,4) = CR3+CI4
         CHI(JJ,1,K,5) = CR2+CI5
  101 CONTINUE
      IDP2 = IDO/2+2
      DO 103 K=1,L1
      DO 103 I=2,IDO/2
        IC = IDP2-I
        DO 102 J=1,M
            JJ=1+JMP*(J-1)
            TI5 = CCI(JJ,I,3,K)+CCI(JJ,IC,2,K)
            TI2 = CCI(JJ,I,3,K)-CCI(JJ,IC,2,K)
            TI4 = CCI(JJ,I,5,K)+CCI(JJ,IC,4,K)
            CI5 = TI11*TI5+TI12*TI4
            CI4 = TI12*TI5-TI11*TI4
            TI3 = CCI(JJ,I,5,K)-CCI(JJ,IC,4,K)
            CI2 = CCI(JJ,I,1,K)+TR11*TI2+TR12*TI3
            CI3 = CCI(JJ,I,1,K)+TR12*TI2+TR11*TI3
            CHI(JJ,I,K,1) = CCI(JJ,I,1,K)+TI2+TI3
            TR5 = CCR(JJ,I,3,K)-CCR(JJ,IC,2,K)
            TR2 = CCR(JJ,I,3,K)+CCR(JJ,IC,2,K)
            TR4 = CCR(JJ,I,5,K)-CCR(JJ,IC,4,K)
            CR5 = TI11*TR5+TI12*TR4
            CR4 = TI12*TR5-TI11*TR4
            TR3 = CCR(JJ,I,5,K)+CCR(JJ,IC,4,K)
            CR2 = CCR(JJ,I,1,K)+TR11*TR2+TR12*TR3
            CR3 = CCR(JJ,I,1,K)+TR12*TR2+TR11*TR3
            CHR(JJ,I,K,1) = CCR(JJ,I,1,K)+TR2+TR3
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            CHR(JJ,I,K,3) = WA2(2*I-3)*DR3-WA2(2*I-2)*DI3
            CHI(JJ,I,K,3) = WA2(2*I-3)*DI3+WA2(2*I-2)*DR3
            CHR(JJ,I,K,4) = WA3(2*I-3)*DR4-WA3(2*I-2)*DI4
            CHI(JJ,I,K,4) = WA3(2*I-3)*DI4+WA3(2*I-2)*DR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CHR(JJ,I,K,2) = WA1(2*I-3)*DR2-WA1(2*I-2)*DI2
            CHI(JJ,I,K,2) = WA1(2*I-3)*DI2+WA1(2*I-2)*DR2
            CHR(JJ,I,K,5) = WA4(2*I-3)*DR5-WA4(2*I-2)*DI5
            CHI(JJ,I,K,5) = WA4(2*I-3)*DI5+WA4(2*I-2)*DR5
  102     CONTINUE
  103 CONTINUE
      DO 106 K=1,L1
      DO 106 J=1,M
         JJ=1+JMP*(J-1)
         T1 = CCR(JJ,IDO/2+1,1,K)+CCR(JJ,IDO/2+1,3,K)
         T2 = .5*T1-CCR(JJ,IDO/2+1,5,K)
         T3 = (2.*TR13)*(CCR(JJ,IDO/2+1,1,K)-CCR(JJ,IDO/2+1,3,K))
         T4 = (2.*TI12)*CCI(JJ,1,2,K)+(2.*TI11)*CCI(JJ,1,4,K) 
         T5 = (2.*TI11)*CCI(JJ,1,2,K)-(2.*TI12)*CCI(JJ,1,4,K)
         T6 = T3+T2
         T7 = T3-T2
         CHR(JJ,IDO/2+1,K,1) = CCR(JJ,IDO/2+1,5,K)+2.*T1
         CHR(JJ,IDO/2+1,K,2) = T6-T4
         CHR(JJ,IDO/2+1,K,3) = T7-T5
         CHR(JJ,IDO/2+1,K,4) = -T7-T5
         CHR(JJ,IDO/2+1,K,5) = -T6-T4
  106 CONTINUE
      RETURN
      END




      SUBROUTINE VRFFTF (CR,CI,WR,WI,N,M,INC,JMP,WSAVE)
      IMPLICIT LOGICAL(A-Z)
      INTEGER INC,JMP,M,N
      REAL CR(1+INC*(N/2)+JMP*(M-1)),CI(1+INC*(N/2)+JMP*(M-1))
      REAL WR(1+INC*(N/2)+JMP*(M-1)),WI(1+INC*(N/2)+JMP*(M-1))
      REAL WSAVE(N+15)
      INTEGER J
      IF (N .EQ. 1) RETURN
      CALL RFFTF1 (CR,CI,WR,WI,N,M,INC,JMP,WSAVE,WSAVE(N+1))
      DO 117 J=1,M
        CI(1+JMP*(J-1)+INC*N/2)=0.
 117  CONTINUE
      RETURN
      END
      SUBROUTINE RFFTF1 (CR,CI,WR,WI,N,M,INC,JMP,WA,IFAC)
      IMPLICIT LOGICAL(A-Z)
      INTEGER N,M,INC,JMP,IFAC(15)
      REAL WA(N)
      REAL CR(1+INC*(N/2)+JMP*(M-1)),CI(1+INC*(N/2)+JMP*(M-1))
      REAL WR(1+INC*(N/2)+JMP*(M-1)),WI(1+INC*(N/2)+JMP*(M-1))
      INTEGER NF,NA,L2,IW,K1,KH,IDO,IDL1,IP,L1,IX2,IX3,IX4,I,J,II
      NF = IFAC(2)
      NA = 1
      L2 = N
      IW = N
      DO 111 K1=1,NF
         KH = NF-K1
         IP = IFAC(KH+3)
         L1 = L2/IP
         IDO = N/L2
         IDL1 = IDO*L1
         IW = IW-(IP-1)*IDO
         NA = 1-NA
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF(IDO.EQ.1) THEN
           IF(IP.EQ.4)CALL RADF4F (L1,INC,JMP,M,CR,CI,WR,WI)
           IF(IP.EQ.2)CALL RADF2F (L1,INC,JMP,M,CR,CI,WR,WI)
         ELSE
           IF(NA .EQ. 0) THEN
             IF(IP.EQ.4)CALL RADF4 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                   WA(IW),WA(IX2),WA(IX3))
             IF(IP.EQ.2)CALL RADF2 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                   WA(IW))
             IF(IP.EQ.3)CALL RADF3 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                   WA(IW),WA(IX2))
             IF(IP.EQ.5)CALL RADF5 (IDO,L1,INC,JMP,M,CR,CI,WR,WI,
     &                WA(IW),WA(IX2),WA(IX3),WA(IX4))
           ELSE
             IF(IP.EQ.4)CALL RADF4 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &                   WA(IW),WA(IX2),WA(IX3))
             IF(IP.EQ.2)CALL RADF2 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &                   WA(IW))
             IF(IP.EQ.3)CALL RADF3 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &          WA(IW),WA(IX2))
             IF(IP.EQ.5)CALL RADF5 (IDO,L1,INC,JMP,M,WR,WI,CR,CI,
     &          WA(IW),WA(IX2),WA(IX3),WA(IX4))
           ENDIF
         ENDIF
           IF(IP.GT.5)THEN
             WRITE(*,*) 'FACTOR ',IP,' NOT IMPLEMENTED'
             STOP
           ENDIF
        L2 = L1
  111 CONTINUE
      IF (NA .EQ. 0) THEN
        DO 112 I=2,N/2
        DO 112 J=1,M
          II=1+JMP*(J-1)+INC*(I-1)
          CR(II) = WR(II)
          CI(II) = WI(II)
  112   CONTINUE
        DO 116 J=1,M
          CR(1+JMP*(J-1))=WI(1+JMP*(J-1))
 116   CONTINUE
        DO 113 J=1,M
          CR(1+JMP*(J-1)+INC*N/2)=WR(1+JMP*(J-1)+INC*N/2)
 113    CONTINUE    
      ELSE
        DO 114 J=1,M
          CR(1+JMP*(J-1))=CI(1+JMP*(J-1))
 114      CONTINUE  
      ENDIF
        DO 115 J=1,M
          CI(1+JMP*(J-1))=0.
 115    CONTINUE
      RETURN
      END


      SUBROUTINE RADF2F (L1,INC,JMP,M,CCR,CCI,CHR,CHI)
      REAL CCR(INC,1),CCI(INC,1)
      REAL  CHR(INC,2,1),CHI(INC,2,1)
      IF(MOD(L1,2).EQ.0) THEN
      DO 101 K=1,L1/2
      DO 101 J=1,M
        JJ=1+JMP*(J-1)
        CHI(JJ,1,K) = CCR(JJ,K)+CCR(JJ,K+L1/2)
        CHR(JJ,2,K) = CCR(JJ,K)-CCR(JJ,K+L1/2)
        CHI(JJ,2,K) = CCI(JJ,K)+CCI(JJ,K+L1/2)
        CHR(JJ,1,K+1) = CCI(JJ,K)-CCI(JJ,K+L1/2)
 101  CONTINUE
      ELSE
      DO 102 K=1,L1/2
      DO 102 J=1,M
        JJ=1+JMP*(J-1)
        CHI(JJ,1,K) = CCR(JJ,K)+CCI(JJ,K+L1/2)
        CHR(JJ,2,K) = CCR(JJ,K)-CCI(JJ,K+L1/2)
        CHI(JJ,2,K) = CCI(JJ,K)+CCR(JJ,K+L1/2+1)
        CHR(JJ,1,K+1) = CCI(JJ,K)-CCR(JJ,K+L1/2+1)
 102    CONTINUE
      DO 103 J=1,M
        JJ=1+JMP*(J-1)
        CHI(JJ,1,L1/2+1) = CCR(JJ,L1/2+1)+CCI(JJ,L1)
        CHR(JJ,2,L1/2+1) = CCR(JJ,L1/2+1)-CCI(JJ,L1)
 103  CONTINUE
      ENDIF
      RETURN
      END

      SUBROUTINE RADF2 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,WA1)
      REAL CHR(INC,IDO/2,2,L1),CHI(INC,IDO/2,2,L1)
      REAL WA1(IDO-1),CCR(INC,IDO/2,L1,2),CCI(INC,IDO/2,L1,2)
      DO 101 K=1,L1
      DO 101 J=1,M
         JJ=1+JMP*(J-1)
         CHI(JJ,1,1,K) = CCI(JJ,1,K,1)+CCI(JJ,1,K,2)
         CHR(JJ,IDO/2+1,2,K) = CCI(JJ,1,K,1)-CCI(JJ,1,K,2)
  101 CONTINUE
      IF (IDO.GT.2) THEN
        IDP2 = IDO/2+2
        DO 104 K=1,L1
        DO 104 I=2,IDO/2
          IC = IDP2-I
          DO 105 J=1,M
            JJ=1+JMP*(J-1)
            TR2 = WA1(2*I-3)*CCR(JJ,I,K,2)+WA1(2*I-2)*CCI(JJ,I,K,2)
            TI2 = WA1(2*I-3)*CCI(JJ,I,K,2)-WA1(2*I-2)*CCR(JJ,I,K,2)
            CHI(JJ,I,1,K) = CCI(JJ,I,K,1)+TI2
            CHI(JJ,IC,2,K) = TI2-CCI(JJ,I,K,1)
            CHR(JJ,I,1,K) = CCR(JJ,I,K,1)+TR2
            CHR(JJ,IC,2,K) = CCR(JJ,I,K,1)-TR2
 105     CONTINUE
 104  CONTINUE
      ENDIF
      DO 106 K=1,L1
      DO 106 J=1,M
        JJ=1+JMP*(J-1)
        CHI(JJ,1,2,K) = -CCR(JJ,IDO/2+1,K,2)
        CHR(JJ,IDO/2+1,1,K) = CCR(JJ,IDO/2+1,K,1)
106   CONTINUE
      RETURN
      END

      SUBROUTINE RADF3 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,WA1,WA2)
      REAL  CHR(INC,IDO/2,3,L1),CHI(INC,IDO/2,3,L1)
      REAL  CCR(INC,IDO/2,L1,3),CCI(INC,IDO/2,L1,3), WA1(1),WA2(1)
      REAL TAUR,TAUI
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
      DO 101 J=1,M
          JJ=1+JMP*(J-1)
          CR2 = CCI(JJ,1,K,2)+CCI(JJ,1,K,3)
          CHI(JJ,1,1,K) = CCI(JJ,1,K,1)+CR2
          CHI(JJ,1,3,K) = TAUI*(CCI(JJ,1,K,3)-CCI(JJ,1,K,2))
          CHR(JJ,IDO/2+1,2,K) = CCI(JJ,1,K,1)+TAUR*CR2
  101 CONTINUE
      IDP2 = IDO/2+2
      DO 102 K=1,L1
      DO 102 I=2,IDO/2
        IC = IDP2-I
        DO 103 J=1,M
          JJ=1+JMP*(J-1)
          DR2 = WA1(2*I-3)*CCR(JJ,I,K,2)+WA1(2*I-2)*CCI(JJ,I,K,2)
          DI2 = WA1(2*I-3)*CCI(JJ,I,K,2)-WA1(2*I-2)*CCR(JJ,I,K,2)
          DR3 = WA2(2*I-3)*CCR(JJ,I,K,3)+WA2(2*I-2)*CCI(JJ,I,K,3)
          DI3 = WA2(2*I-3)*CCI(JJ,I,K,3)-WA2(2*I-2)*CCR(JJ,I,K,3)
C          CR2 = DR2+DR3
C          CI2 = DI2+DI3
          CHR(JJ,I,1,K) = CCR(JJ,I,K,1)+(DR2+DR3)
          TR2 = CCR(JJ,I,K,1)+TAUR*(DR2+DR3)
          CHI(JJ,I,1,K) = CCI(JJ,I,K,1)+(DI2+DI3)
          TI2 = CCI(JJ,I,K,1)+TAUR*(DI2+DI3)
          TR3 = TAUI*(DI2-DI3)
          TI3 = TAUI*(DR3-DR2)
          CHR(JJ,I,3,K) = TR2+TR3
          CHR(JJ,IC,2,K) = TR2-TR3
          CHI(JJ,I,3,K) = TI2+TI3
          CHI(JJ,IC,2,K) = TI3-TI2
 103    CONTINUE
 102  CONTINUE
      DO 106 K=1,L1
      DO 106 J=1,M
         JJ=1+JMP*(J-1)
         T1 = CCR(JJ,IDO/2+1,K,2)-CCR(JJ,IDO/2+1,K,3)
         CHR(JJ,IDO/2+1,1,K) = .5*T1+CCR(JJ,IDO/2+1,K,1)
         CHR(JJ,IDO/2+1,3,K) = CCR(JJ,IDO/2+1,K,1)-T1
         CHI(JJ,1,2,K) = -TAUI*(CCR(JJ,IDO/2+1,K,2)+CCR(JJ,IDO/2+1,K,3))
  106 CONTINUE
      RETURN
      END
      SUBROUTINE RADF4F (L1,INC,JMP,M,CCR,CCI,CHR,CHI)
      REAL CCR(INC,2*L1+1),CCI(INC,2*L1),CHR(INC,4,L1+1),CHI(INC,4,L1)
      IF(MOD(L1,2).EQ.0) THEN
        DO 101 K=1,L1/2
        DO 101 J=1,M
          JJ=1+JMP*(J-1)
          TR1 = CCR(JJ,K+L1/2)+CCR(JJ,K+3*L1/2)
          CHI(JJ,2,K) = CCR(JJ,K+3*L1/2)-CCR(JJ,K+L1/2)
          TR2 = CCR(JJ,K)+CCR(JJ,K+L1)
          CHR(JJ,2,K) = CCR(JJ,K)-CCR(JJ,K+L1)
          CHI(JJ,1,K) = TR1+TR2
          CHR(JJ,3,K) = TR2-TR1
          TR1 = CCI(JJ,K+L1/2)+CCI(JJ,K+3*L1/2)
          CHI(JJ,4,K) = CCI(JJ,K+3*L1/2)-CCI(JJ,K+L1/2)
          TR2 = CCI(JJ,K)+CCI(JJ,K+L1)
          CHR(JJ,4,K) = CCI(JJ,K)-CCI(JJ,K+L1)
          CHI(JJ,3,K) = TR1+TR2
          CHR(JJ,1,K+1) = TR2-TR1
 101    CONTINUE
      ELSE
        DO 102 K=1,L1/2
        DO 102 J=1,M
          JJ=1+JMP*(J-1)
          TR1 = CCI(JJ,K+L1/2)+CCI(JJ,K+3*L1/2)
          CHI(JJ,2,K) = CCI(JJ,K+3*L1/2)-CCI(JJ,K+L1/2)
          TR2 = CCR(JJ,K)+CCR(JJ,K+L1)
          CHR(JJ,2,K) = CCR(JJ,K)-CCR(JJ,K+L1)
          CHI(JJ,1,K) = TR1+TR2
          CHR(JJ,3,K) = TR2-TR1
          TR1 = CCR(JJ,K+L1/2+1)+CCR(JJ,K+3*L1/2+1)
          CHI(JJ,4,K) = CCR(JJ,K+3*L1/2+1)-CCR(JJ,K+L1/2+1)
          TR2 = CCI(JJ,K)+CCI(JJ,K+L1)
          CHR(JJ,4,K) = CCI(JJ,K)-CCI(JJ,K+L1)
          CHI(JJ,3,K) = TR1+TR2
          CHR(JJ,1,K+1) = TR2-TR1
 102    CONTINUE
        DO 103 J=1,M
          JJ=1+JMP*(J-1)
          TR1 = CCI(JJ,L1)+CCI(JJ,2*L1)
          TR2 = CCR(JJ,L1/2+1)+CCR(JJ,3*L1/2+1)
          CHI(JJ,1,L1/2+1) = TR1+TR2
          CHR(JJ,3,L1/2+1) = TR2-TR1
          CHR(JJ,2,L1/2+1) = CCR(JJ,L1/2+1)-CCR(JJ,3*L1/2+1)
          CHI(JJ,2,L1/2+1) = CCI(JJ,2*L1)-CCI(JJ,L1)
 103    CONTINUE
      ENDIF
      RETURN
      END


      SUBROUTINE RADF4 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,WA1,WA2,WA3)
      REAL CCR(INC,IDO/2,L1,4),CCI(INC,IDO/2,L1,4)
      REAL CHR(INC,IDO/2,4,L1),CHI(INC,IDO/2,4,L1)
      REAL WA1(1)     ,WA2(1)     ,WA3(1)
      DATA HSQT2 /.7071067811865475/
      DO 101 K=1,L1
      DO 101 J=1,M
         JJ=1+JMP*(J-1)
         TR1 = CCI(JJ,1,K,2)+CCI(JJ,1,K,4)
         TR2 = CCI(JJ,1,K,1)+CCI(JJ,1,K,3)
         CHI(JJ,1,1,K) = TR1+TR2
         CHR(JJ,IDO/2+1,4,K) = TR2-TR1
         CHR(JJ,IDO/2+1,2,K) = CCI(JJ,1,K,1)-CCI(JJ,1,K,3)
         CHI(JJ,1,3,K) = CCI(JJ,1,K,4)-CCI(JJ,1,K,2)
  101 CONTINUE
      IF (IDO.GT.2) THEN
  102   IDP2 = IDO/2+2
        DO 104 K=1,L1
        DO 104 I=2,IDO/2
          IC = IDP2-I
          DO 103 J=1,M
            JJ=1+JMP*(J-1)
            CR2 = WA1(2*I-3)*CCR(JJ,I,K,2)+WA1(2*I-2)*CCI(JJ,I,K,2)
            CI2 = WA1(2*I-3)*CCI(JJ,I,K,2)-WA1(2*I-2)*CCR(JJ,I,K,2)
            CR4 = WA3(2*I-3)*CCR(JJ,I,K,4)+WA3(2*I-2)*CCI(JJ,I,K,4)
            CI4 = WA3(2*I-3)*CCI(JJ,I,K,4)-WA3(2*I-2)*CCR(JJ,I,K,4)
C            TR1 = CR2+CR4
C            TR4 = CR4-CR2
C            TI1 = CI2+CI4
C            TI4 = CI2-CI4
            CR3 = WA2(2*I-3)*CCR(JJ,I,K,3)+WA2(2*I-2)*CCI(JJ,I,K,3)
            CI3 = WA2(2*I-3)*CCI(JJ,I,K,3)-WA2(2*I-2)*CCR(JJ,I,K,3)
            TI2 = CCI(JJ,I,K,1)+CI3
            TI3 = CCI(JJ,I,K,1)-CI3
            CHI(JJ,I,1,K) = (CI2+CI4)+TI2
            CHI(JJ,IC,4,K) = (CI2+CI4)-TI2
            CHI(JJ,I,3,K) = (CR4-CR2)+TI3
            CHI(JJ,IC,2,K) = (CR4-CR2)-TI3
C            TR2 = CCR(JJ,I,K,1)+CR3
C            TR3 = CCR(JJ,I,K,1)-CR3
            CHR(JJ,I,1,K) = (CR2+CR4)+(CCR(JJ,I,K,1)+CR3)
            CHR(JJ,IC,4,K) = (CCR(JJ,I,K,1)+CR3)-(CR2+CR4)
            CHR(JJ,I,3,K) = (CI2-CI4)+(CCR(JJ,I,K,1)-CR3)
            CHR(JJ,IC,2,K) = (CCR(JJ,I,K,1)-CR3)-(CI2-CI4)
  103     CONTINUE
  104   CONTINUE
      ENDIF
      DO 106 K=1,L1
      DO 106 J=1,M
         JJ=1+JMP*(J-1)
         TI1 = -HSQT2*(CCR(JJ,IDO/2+1,K,2)+CCR(JJ,IDO/2+1,K,4))
         TR1 = HSQT2*(CCR(JJ,IDO/2+1,K,2)-CCR(JJ,IDO/2+1,K,4))
         CHR(JJ,IDO/2+1,1,K) = TR1+CCR(JJ,IDO/2+1,K,1)
         CHR(JJ,IDO/2+1,3,K) = CCR(JJ,IDO/2+1,K,1)-TR1
         CHI(JJ,1,2,K) = TI1-CCR(JJ,IDO/2+1,K,3)
         CHI(JJ,1,4,K) = TI1+CCR(JJ,IDO/2+1,K,3)
  106 CONTINUE
  107 RETURN
      END

      SUBROUTINE RADF5 (IDO,L1,INC,JMP,M,CCR,CCI,CHR,CHI,
     &                  WA1,WA2,WA3,WA4)
      REAL CCR(INC,IDO/2,L1,5),CCI(INC,IDO/2,L1,5)
      REAL CHR(INC,IDO/2,5,L1),CHI(INC,IDO/2,5,L1)
      REAL WA1(1)     ,WA2(1)     ,WA3(1)     ,WA4(1)
      DATA TR11,TI11,TR12,TI12,TR13 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473,.55901699437495/
      DO 101 K=1,L1
      DO 101 J=1,M
        JJ=1+JMP*(J-1)
        CR2 = CCI(JJ,1,K,5)+CCI(JJ,1,K,2)
        CI5 = CCI(JJ,1,K,5)-CCI(JJ,1,K,2)
        CI4 = CCI(JJ,1,K,4)-CCI(JJ,1,K,3)
        CHI(JJ,1,3,K) = TI11*CI5+TI12*CI4
        CHI(JJ,1,5,K) = TI12*CI5-TI11*CI4
        CR3 = CCI(JJ,1,K,4)+CCI(JJ,1,K,3)
        CHI(JJ,1,1,K) = CCI(JJ,1,K,1)+CR2+CR3
        CHR(JJ,IDO/2+1,2,K) = CCI(JJ,1,K,1)+TR11*CR2+TR12*CR3
        CHR(JJ,IDO/2+1,4,K) = CCI(JJ,1,K,1)+TR12*CR2+TR11*CR3
 101  CONTINUE
      IF(IDO.GT.2) THEN
        IDP2 = IDO/2+2
        DO 103 K=1,L1
        DO 103 I=2,IDO/2
          IC = IDP2-I
          DO 102 J=1,M
            JJ=1+JMP*(J-1)
            DR2 = WA1(2*I-3)*CCR(JJ,I,K,2)+WA1(2*I-2)*CCI(JJ,I,K,2)
            DI2 = WA1(2*I-3)*CCI(JJ,I,K,2)-WA1(2*I-2)*CCR(JJ,I,K,2)
            DR3 = WA2(2*I-3)*CCR(JJ,I,K,3)+WA2(2*I-2)*CCI(JJ,I,K,3)
            DI3 = WA2(2*I-3)*CCI(JJ,I,K,3)-WA2(2*I-2)*CCR(JJ,I,K,3)
            DR4 = WA3(2*I-3)*CCR(JJ,I,K,4)+WA3(2*I-2)*CCI(JJ,I,K,4)
            DI4 = WA3(2*I-3)*CCI(JJ,I,K,4)-WA3(2*I-2)*CCR(JJ,I,K,4)
            DR5 = WA4(2*I-3)*CCR(JJ,I,K,5)+WA4(2*I-2)*CCI(JJ,I,K,5)
            DI5 = WA4(2*I-3)*CCI(JJ,I,K,5)-WA4(2*I-2)*CCR(JJ,I,K,5)
            CR2 = DR2+DR5
            CI5 = DR5-DR2
            CR3 = DR3+DR4
            CI4 = DR4-DR3
            CHR(JJ,I,1,K) = CCR(JJ,I,K,1)+CR2+CR3
            TR2 = CCR(JJ,I,K,1)+TR11*CR2+TR12*CR3
            TR3 = CCR(JJ,I,K,1)+TR12*CR2+TR11*CR3
            CR5 = DI2-DI5
            CI2 = DI2+DI5
            CR4 = DI3-DI4
            CI3 = DI3+DI4
            CHI(JJ,I,1,K) = CCI(JJ,I,K,1)+CI2+CI3
            TI2 = CCI(JJ,I,K,1)+TR11*CI2+TR12*CI3
            TI3 = CCI(JJ,I,K,1)+TR12*CI2+TR11*CI3
            TR5 = TI11*CR5+TI12*CR4
            TR4 = TI12*CR5-TI11*CR4
            CHR(JJ,I,5,K) = TR3+TR4
            CHR(JJ,IC,4,K) = TR3-TR4
            CHR(JJ,I,3,K) = TR2+TR5
            CHR(JJ,IC,2,K) = TR2-TR5
            TI5 = TI11*CI5+TI12*CI4
            TI4 = TI12*CI5-TI11*CI4
            CHI(JJ,I,3,K) = TI2+TI5
            CHI(JJ,IC,2,K) = TI5-TI2
            CHI(JJ,I,5,K) = TI3+TI4
            CHI(JJ,IC,4,K) = TI4-TI3
  102     CONTINUE
  103   CONTINUE
      ENDIF
      IF (MOD(IDO,2) .EQ. 1) RETURN
      DO 106 K=1,L1
      DO 106 J=1,M
         JJ=1+JMP*(J-1)
         T2 = CCR(JJ,IDO/2+1,K,2)+CCR(JJ,IDO/2+1,K,5)
         T1 = CCR(JJ,IDO/2+1,K,2)-CCR(JJ,IDO/2+1,K,5)
         T4 = CCR(JJ,IDO/2+1,K,3)+CCR(JJ,IDO/2+1,K,4)
         T3 = CCR(JJ,IDO/2+1,K,3)-CCR(JJ,IDO/2+1,K,4)
         T5 = T1-T3
         T6 = CCR(JJ,IDO/2+1,K,1)+.25*T5
         T7 = TR13*(T1+T3)
         CHR(JJ,IDO/2+1,1,K) = T6+T7 
         CHR(JJ,IDO/2+1,3,K) = T6-T7
         CHR(JJ,IDO/2+1,5,K) = CCR(JJ,IDO/2+1,K,1)-T5
         CHI(JJ,1,2,K) = -TI12*T2-TI11*T4
         CHI(JJ,1,4,K) = -TI11*T2+TI12*T4
  106 CONTINUE
      RETURN
      END









      SUBROUTINE VRFFTI (N,WSAVE,IFAIL)
      DIMENSION       WSAVE(N+15)
      IF (N .EQ. 1) THEN
        IFAILN=1
      ELSE
        CALL RFFTI1 (N,WSAVE(1),WSAVE(N+1),IFAILN)
      ENDIF
      IF(IFAIL.EQ.0.AND.IFAILN.NE.0) THEN
        IF(IFAILN.EQ.1) THEN
          WRITE(*,*) 'VRFFTI-ERROR-N MUST BE EVEN'
        ELSE
          WRITE(*,*) 'VRFFTI-ERROR-CANNOT FACTOR',IFAILN
        ENDIF
        STOP
      ENDIF
      IF(IFAIL.NE.0) IFAIL=IFAILN    
      RETURN
      END
      SUBROUTINE RFFTI1 (N,WA,IFAC,IFAIL)
      DIMENSION       WA(N),IFAC(15),NTRYH(4),NFAC(4)
      DATA NTRYH/2,3,4,5/
      IFAIL=0
      NL = N
      NF = 0
C FIRST FACTOR N, SELECT LARGE FACTORS FIRST
      DO 101 J=4,1,-1
        NFAC(J)=0
        NTRY=NTRYH(J)
 102    IF (NL.EQ.NL/NTRY*NTRY) THEN
          NL=NL/NTRY
          NF=NF+1
          NFAC(J)=NFAC(J)+1
          GOTO 102
        ENDIF
 101  CONTINUE
      IF(NL.NE.1) THEN
        IFAIL=NL
        RETURN
      ENDIF
C NOW INSERT THE FACTORS INTO IFAC WITH THE LARGEST EVEN FACTOR LAST
      IF(NFAC(3).GT.0) THEN
        IFAC(NF+2)=4
        NFAC(3)=NFAC(3)-1
      ELSE
        IF(NFAC(1).GT.0) THEN
          IFAC(NF+2)=2
          NFAC(1)=NFAC(1)-1
        ELSE
          IFAIL=1
          RETURN
        ENDIF
      ENDIF         
      NP=3
      DO 103 J=1,4
        DO 104 NP=NP,NP-1+NFAC(J)
          IFAC(NP)=NTRYH(J)
 104    CONTINUE
 103  CONTINUE
      IFAC(1) = N
      IFAC(2) = NF
      TPI = 6.28318530717959
      ARGH = TPI/FLOAT(N)
      IS = 0
      NFM1 = NF-1
      L1 = 1
      IF (NFM1 .EQ. 0) RETURN
      DO 110 K1=1,NFM1
         IP = IFAC(K1+2)
         LD = 0
         L2 = L1*IP
         IDO = N/L2
         IPM = IP-1
         DO 109 J=1,IPM
            LD = LD+L1
            I = IS
            ARGLD = FLOAT(LD)*ARGH
            FI = 0.
            DO 108 II=3,IDO,2
               I = I+2
               FI = FI+1.
               ARG = FI*ARGLD
               WA(I-1) = COS(ARG)
               WA(I) = SIN(ARG)
  108       CONTINUE
            IS = IS+IDO
  109    CONTINUE
         L1 = L2
  110 CONTINUE
      RETURN
      END










      SUBROUTINE VSINT (X,W,N,M,INC,JMP,WSAVE)
      DIMENSION       X(1)       ,WSAVE(1)
      NP1 = N+1
      IW1 = N/2+1
      IW2 = IW1+NP1
      CALL SINT1(X,W,N,M,INC,JMP,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE SINT1(X,W,N,M,INC,JMP,WAS,WAR,IFAC)
      DIMENSION WAR(1),WAS(1),X(INC,1),W(INC,1),IFAC(1)
      DATA SQRT3 /1.73205080756888/
      IF(N.EQ.1) THEN
        DO 200 J=1,M
          JJ=1+(J-1)*JMP
          X(JJ,1) = X(JJ,1)+X(JJ,1)
 200    CONTINUE
      ELSE
      NP1 = N+1
      NS2 = N/2
      DO 205 J=1,M
        JJ=1+(J-1)*JMP
        W(JJ,1) = 0.
 205  CONTINUE
      DO 104 K=1,NS2
        KC = NP1-K
CDIR$ IVDEP
        DO 202 J=1,M
          JJ=1+(J-1)*JMP
          T1 = X(JJ,K)-X(JJ,KC)
          T2 = WAS(K)*(X(JJ,K)+X(JJ,KC))
	  W(JJ,K+1) = T1+T2
	  W(JJ,KC+1) = T2-T1
  202   CONTINUE
  104 CONTINUE
      DO 203 J=1,M
        JJ=1+(J-1)*JMP
        W(JJ,NS2+2) = 4.*X(JJ,NS2+1)
 203  CONTINUE
      CALL RFFTF1 (W,W(1,2),X,X(1,2),NP1,M,2*INC,JMP,WAR,IFAC)
      DO 204 J=1,M
        JJ=1+(J-1)*JMP
        X(JJ,1) = .5*W(JJ,1)
 204  CONTINUE
      DO 105 I=3,N,2
CDIR$ IVDEP
      DO 105 J=1,M
         JJ=1+(J-1)*JMP
	 X(JJ,I-1) = -W(JJ,I+1)
	 X(JJ,I) = X(JJ,I-2)+W(JJ,I)
  105 CONTINUE
      RETURN
      ENDIF
      END

      SUBROUTINE VSINTI (N,WSAVE,IFAIL)
      DIMENSION       WSAVE(3*N/2+15)
      DATA PI /3.14159265358979/
      IFAILN=0
      IF (N .NE. 1) THEN
        NS2 = N/2
        NP1 = N+1
        DT = PI/FLOAT(NP1)
        DO 101 K=1,NS2
          WSAVE(K) = 2.*SIN(K*DT)
  101   CONTINUE
        CALL RFFTI1 (NP1,WSAVE(NS2+1),WSAVE(NS2+N+2),IFAILN)
        IF(IFAIL.EQ.0.AND.IFAILN.NE.0) THEN
          IF(IFAILN.EQ.1) THEN
            WRITE(*,*) 'VSINTI-ERROR-N MUST BE ODD'
          ELSE
            WRITE(*,*) 'VSINTI-ERROR-CANNOT FACTOR',IFAILN
          ENDIF
          STOP
        ENDIF
      ENDIF
      IF(IFAIL.NE.0) IFAIL=IFAILN    
      RETURN
      END
