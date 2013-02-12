BIDMAT_ROOT="/Users/dgreis/Documents/School/Classes/CS_294/BIDMat"
BIDMAT_LIBS="${BIDMAT_ROOT}/BIDMat.jar:${BIDMAT_ROOT}/lib/ptplot.jar:${BIDMAT_ROOT}/lib/ptplotapplication.jar:${BIDMAT_ROOT}/lib/jhdf5.jar:${BIDMAT_ROOT}/lib/commons-math3-3.1.1.jar"
ALL_LIBS="${BIDMAT_LIBS}:${JAVA_HOME}/lib/tools.jar"
scala -cp "${ALL_LIBS}" $1
