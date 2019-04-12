# 분포에 대한 허용한계(tolerance limit)구하기

# 허용한계는 확률의 확률을 의미한다. 요컨데, 어떤 확률변수 X에 대하여 X가 cdf(x;c)를 따를때
# 이 확률변수가 속한 확률구간이 실제로 실현될 확률을 나타내는 것이다.
# 예를 들어, 어떤 회사에서 제품마다 정확히 12kg이 담겨야 한다고 했을 때
# mu = 12kg인 X에 대한 99% 확률구간이 95% 실현될 수 있는 상한과 하한 (Xi, Xj)를 허용 구간이라고 하고
# 이때 (Xi, Xj)를 X의 99% 확률구간에 대한 95% 허용한계라고 한다.

#X 라는 확률변수 단독에 대해서는 허용한계값이 균등분포를 따르기 때문에,
#P(F(X)>=p)=p이다. 즉, F(X)가 p보다 클 확률은 p 자기 자신이 바로 허용한계이다.

#X에서 확률표본 X1...Xn을 선출하고, 이 X1...Xn을 순서통계량 Y1...Yn으로 변환하면
#이때부터는 순서통계량 F(Y1)...F(Yn)을 고려하기 때문에, 순서통계량의 성질에 따라 확률에 대한 확률 분포가 균등분포에서 베타분포로 바뀐다.
#즉 Y1....Yn ---(분포확률)--> F(Y1)....F(Yn) ----(확률에 대한 확률변수)--->Z1....Zn으로 3단계 변화한다.

#1. Y1과 Yn을 각각 분포함수 F(x)를 따르는 연속형 분포에서 추출한 크기 n인 확률표본의 순서통계량이라고 하자.
#P[F(Yn) - F(Y1) >= 0.5]가 적어도 0.95가 되도록 하는 n의 최솟값을 구하시오

##1) 순서통계량을 고려한 F(Yn)=Z의 pdf는 다음과 같다.
(gamma(n+1)/(gamma(n-1)*gamma(n-n-1+1)))*v^(n-1-1)(1-v)^(n-n+1)
##2) 크기는 주어지지 않았지만, 허용한계는 주어졌으므로, 이를 역으로 이용하면
integrate(f=(1/gamma(2))*v^(n-2)*(1-v)),from=0, to = 0.5)
##가 0.95가 되어야 한다.

