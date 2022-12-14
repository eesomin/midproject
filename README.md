# [2팀 DATA] 중간 프로젝트

주제 : OTT 서비스와 영화관 이용의 관계분석

## 1. 프로젝트 소개

OTT 플랫폼/컨텐츠 이용 패턴 및 이용률에 대한 탐색과 영화관 이용과의 관계 분석

1) 서론

- 선정이유 및 OTT 서비스 현황발표

2) 본론

- OTT 이용과 관련된 이용 분석 및 이용자 탐색
- 가설 통계 분석

3) 결론

- 본론의 분석내용을 바탕으로 한 마케팅 타겟 선정

## 2. 데이터 출처

사용한 데이터 소스는 아래와 같다

- 미디어 통계포털 : [https://stat.kisdi.re.kr/](https://stat.kisdi.re.kr/)
- 통계청 : [https://kostat.go.kr/](https://kostat.go.kr/)
- 영화관입장권 통합 전산망 : [https://www.kobis.or.kr/](https://www.kobis.or.kr/)
- 한국데이터거래소 : [https://kdx.kr/main](https://kdx.kr/main)

## (1) 서론

- 코로나 시대, 거리두기 방역정책으로 인한 공공시설 이용에 제한으로 문화생활을 누릴수 있는 기회가  축소됐으나 역으로, 비대면으로 문화생활을 즐길 수 있는 특히나 OTT 플랫폼의 폭발적 성장이 두드러짐
- 하지만 백신접종 및 방역정책의 완화는 다시금 사람들을 밖으로 나가게 하는 요인으로 작용하여 OTT 플랫폼의 성장은 잠시 주춤하는 상태
- 이에 데이터를 통한 OTT 서비스 전반에 대한 이용 분석과 이용자에 대한 분석을 통해 코로나 시기에 어떤 변화가 있었는지 분석하고 앞으로의 포스트 코로나 시기에  OTT플랫폼의 성장에 영향을 줄 요인을 분석 해본다.
    
    
![노션 2](https://user-images.githubusercontent.com/37919866/204969430-50ef280c-5104-4f65-94f6-47e043f97393.png)    

![노션1](https://user-images.githubusercontent.com/37919866/204973494-149d6324-54c5-4a11-a842-bb24b377ca6c.png)


## (2) 본론

### 2-1. OTT 서비스 이용/ 유저 분석


<img src="https://user-images.githubusercontent.com/37919866/204973581-ea4f4981-1834-409b-b64d-eaa1fdf6b00e.png"  width="350" height="350">      <img src="https://user-images.githubusercontent.com/37919866/204973587-aa00f27a-2b2c-4b63-9067-43db3e90ac4c.png"  width="350" height="350">


2020년과 2021년 전연령에 걸쳐 이용경험 증가를 보이고 2018 년도 잠시 주춤했던 OTT 플랫폼 앱 이용률도 2019년을 시작으로 눈에띄게 증가 했음을 확인할 수 있음


<img src="https://user-images.githubusercontent.com/37919866/204973653-7930a173-c7a3-445e-a000-d564d316544d.png"  width="400" height="350"> <img src="https://user-images.githubusercontent.com/37919866/204973661-bc33bc89-816d-48af-aa82-c7a690eea961.png"  width="350" height="400"> 

2019년에 비해 2020년도의 OTT 유료결제율이 높고 특히나 이용층은 2030세대로 나타났으며, 소득수준별 OTT 이용경험으로 봤을때 소득수준이 없는 단위도 비율이 높은것으로 봐서, 미성년층의 이용률 또한 높을것으로 추정할 수 있다.

OTT 이용이 증가한것을 데이터를 통해 확인해 봤다. 그렇다면 반대로 코로나로 인해 운영에 타격받았을 영화관은 실제로 어떠한가?


<img src="https://user-images.githubusercontent.com/37919866/204973700-d304f8cb-ad74-4c97-8e09-e1db1e872a5a.png"  width="400" height="350">    <img src="https://user-images.githubusercontent.com/37919866/204973712-fc758288-970e-4e91-bbff-2bfa355f71fc.png"  width="400" height="350"> 



2020년을 기준으로 관람객 및 매출액이 급감한것을 확인할수 있다. 

### 2-2. 통계분석

앞선 분석내용을 정리해 보자면 코로나로 인해 영화관 이용이 감소했고 이를 대체 할 수 있는 비대면 문화생활매체로 OTT 플랫폼이 성장했다고 추정할 수 있는데 이를 상관관계 분석을 통해 직접 확인하고자 한다.

- 코로나 확진자와 영화관 관람객의 상관관계
- 영화관 관람객과 OTT 영화 콘텐츠 유저 상관관계

- 2-2-1. 코로나 확진자와 영화관 관람객의 상관관계

가설 : 코로나 확진자가 늘어날수록 영화관 관람객은 감소할 것이다.

코로나의 영향을 코로나의 확진자수로 확인할 수 있기때문에 코로나의 확진자 수가 증가율이 실제로 영화관 괌람객과 얼마나 상관이 있는가 상관관계 분석을 통해 알아봄


<img src="https://user-images.githubusercontent.com/37919866/204973753-2c9e1d3e-12c9-48ef-aaff-c4901064b816.png"  width="400" height="350">  <img src="https://user-images.githubusercontent.com/37919866/204973761-297c22a4-3e4f-4d83-8d31-6dbef0c03ae8.png"  width="400" height="350">



시각화를 통해 확진자와 영화관 관람객은 같은 달 서로 영향을 끼치는것이 아니라 지난달의 확진자수의 증감이 이번달 영화관 이용에 영향을 준다는 것을 확인, 즉 지난달 확진자수가 많으면 이에 대한 영향은 다음달 영화관 이용이 감소될 것이라고 추정 


<img src="https://user-images.githubusercontent.com/37919866/204973797-42161e84-f2ed-43ab-b7cc-f22e80109eaf.png"  width="400" height="350">


2020년도에는 추정대로 약한 상관관계이나 음의 관계, 확진자수가 많을수록 영화관 이용이 감소되는 것을 확인


<img src="https://user-images.githubusercontent.com/37919866/204973806-318d5adc-54e7-4bbc-8bbb-9ba6eafe46c2.png"  width="400" height="350">


하지만, 2021년도에는 오히려 양의 상관관계 추정과는 다르게 확진자수가 늘면 영화관 관람객 또한 증가한다고 나타남

무엇이 이런 연도별 상반된 결과를 나타나게 했을것인가?

![노션14](https://user-images.githubusercontent.com/37919866/204973813-448b3f90-257c-489f-ac8d-6b7a9fc71176.png)


           20년도 키워드 : 위기 치료 방역                                   21년도 키워드 : 축제 회복 활동

20년도 21년도 뉴스 크롤링을 통해 워드클라우드를 구성해본 결과, 20년도에는 코로나 극복을 위한 치료 방역과 코로나 상황의 위기 등과 같은 키워드가 주를 이뤘다면 21년도에는 축제, 회복, 활동 등과 같은 새로운 키워드가 등장, 21년도에는 방역 규제가 완화 됐고 , 긴 코로나 기간 동안의 활동의 제약에 피로를 느낀 사람들이 대면 활동을 원한다는 것을 살펴 볼 수 있었다. 길었던 코로나 방역과의 전쟁에서 사람들의 인식이 변했고 이것이 실제 활동으로 나타났다고 해석할 수 있다. 

- 2-2-2. 영화관 관람객과 OTT 영화 콘텐츠 유저 상관관계

가설 : 영화관 이용이 감소할 수록 OTT영화 콘텐츠 시청 증가율이 증가   

영화관 상영이 감소하면서 영화시장이 전체적으로 어려워 졌는데 이러한 현황이 OTT 플랫폼의 영화 콘텐츠 이용율 증가에 어떤 상관관계를 보일까 상관관계 분석을 통해 알아봄


<img src="https://user-images.githubusercontent.com/37919866/204973981-bc72fb0c-0870-4d52-a7ed-6c538ec83709.png"  width="350" height="350">  <img src="https://user-images.githubusercontent.com/37919866/204973933-723afcc0-3bbd-495d-9908-a61ba22d2bef.png"  width="450" height="350">

OTT 영화 콘텐츠를 이용하는 유저의 연령대를 살펴보면 10대 미만 단위와 70대 이상 단위가 타 연령대에 비해 현저하게 적기때문에 이를 제외, 연령별 OTT 영화시청 증감률과 영화관 이용 증감률 상관관계 분석 진행, P-VALUE 0.45의 음의 상관관계를 보이는 것으로 나타났다.

## (3) 결론

정리하자면 코로나 영향으로 영화관 관람객이 줄면서 OTT 플랫폼 콘텐츠, 특히 영화 콘텐츠 감상이 증가할 것 같지만 연도별 확진자와 영화관 이용객의 증가율의 상관관계 분석을 통해 살펴본 바와 같이 사람들의 코로나에 대한 인식이 변한 탓에  OTT 플랫폼의 성장이 전과 같지는 않을 것이라고 볼 수 있다. 따라서 변화된 사람들의 행동 패턴에 따른 새분화된 전략 구축이 필요하다.
