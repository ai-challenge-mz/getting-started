# AI Challenge от Machine Zone Inc. [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ai-challenge-mz/getting-started?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

*Кратко: требуется реализовать бота для игры в войну планет*

Игра базируется на игре, использованной для [Google AI challenge](http://planetwars.aichallenge.org/specification.php)

Игровой мир состоит из планет, каждая из которых имеет координаты, содержит целое количество кораблей и принадлежит игроку или нейтральна. Игроки ходят одновременно, отправляя флот со своих планет на любые другие. Флот достигает планеты назначения через число ходов, вычисляемое как ![sqr](https://dl.dropboxusercontent.com/u/11486892/mz_sqr.png), после чего происходит битва за планету, которая решается в пользу превосходящей силы.

### Планеты

Каждая планета имеет ID>0, координаты, количество производимых кораблей за ход (может быть отрицательным), ID игрока и количество кораблей на данный момент. Нейтральные планеты имеют ID=0 и флот на них не увеличивается.

### Увеличение продуктивности планет

Каждый ход игрок может увеличить производимое на планете количество кораблей, потратив находящийся на ней флот. Увеличение продуктивности на 1 достигается, если потратить 2 в степени p кораблей, где p -- абсолютное значение текущей продуктивности. Например, если планета производит за ход 1 корабль, то для увеличения на 1 потребуется потратить 2 корабля, а если планета производит 4 корабля за ход, то потребуется 16 кораблей, чтобы продуктивность стала равна пяти.

Попытка увеличить продуктивность одной и той же планеты более одного раза за ход считается незаконной и карается дисквалификацией.

### Обмен информацией с ботом

После каждого хода игровой мир присылается в stdin боту в виде списка планет и его ID в следующем виде:

```
P 1 2 3 1 0 1
P 3 0 0 2 1 1
P 2 3 4 2 2 1
Y 1
.
```

`P <PlanetId> <X> <Y> <GrowthRate> <PlayerId> <NumShips>` - идентификатор планеты, где GrowthRate – прирост флота за ход, NumShips  – количество находящихся на планете кораблей

`Y <PlayerId>` – идентификатор текущего игрока. Это число не меняется в течение игры.

После этого, на принятие решения отводится 1 секунда, ответ бота состоит из приказаний флоту, а так же сообщения о строительстве:

```
F 3 1 1
B 1
.
```

`F <SrcPlanetId> <DstPlanetId> <NumShips>` - приказ флоту, где SrcPlanetId – ID начальной планеты, DstPlanetId - ID планеты назначения, NumShips  – количество кораблей

`B <PlanetId>` – сообщение об увеличении продуктивности

Сообщения вычитываются из stdout бота заканчиваются точкой на новой строке. При этом процесс бота не завершается, через некоторое время ему отправляется обновлённое состояние мира и так пока не закончится игра. Бот, отправивший неправильное сообщение или вышедший за лимит по времени на ход, считается проигравшим.

### Сражения

Сражением считается ситуация, когда на планету прибывает как минимум один флот, не принадлежащий игроку, владеющему планетой (нейтральные планеты условно принадлежат игроку 0).

За ход на планете может произойти только одно сражение, в нем будут участвовать все флоты, прибывающие в этот ход на эту планету. Возможны трёхсторонние сражения, если на нейтральную планету в один ход прибывают флоты двух игроков.

Собственно сражение происходит так: сначала все флоты (в том числе наземный) объединяются по принадлежности игроку, получается две или три армии. Если среди них есть единственная армия с наибольшей численностью, то планета по окончании сражения будет принадлежать ей, а количество кораблей будет равно разности между численностью этой армии и второй по величине, участвовавшей в этом же сражении. Если единственной превосходящей армии в сражении нет, то планета владельца не меняет, но количество кораблей на ней становится равным нулю.

### Последовательность вычисления

В пределах одного хода сначала происходит посылка состояния мира ботам и принятие приказов от них, затем выполнение приказов, потом увеличение численности флота на планетах и потом сражения. Притом, мир не зависит от порядка опроса ботов, все получают одинаковое состояние.

### Окончание игры

Игра заканчивается, если у кого-то из игроков нет кораблей (если у игрока нет планет, но есть флот в пути - это еще не проигрыш), либо кто-то из игроков совершил незаконный ход или не смог ответить за одну секунду, либо если превышен лимит на количество ходов 200. В последнем случае выигрывает игрок с наибольшим флотом.

### Размеры мира

Предполагается тестировать решения на мирах, не превышающих 100 планет.

### Время и старт бота

В зависимости от используемых технологий при написании бота для его запуска может потребоваться дополнительное время до 10 секунд.

## Getting started

Мы подготовили несколько простых реализаций бота на разных ЯП. Мы верим, что это поможет вам сэкономить время на парсинге входных данных и сосредоточиться на задаче.

Боты реализованы на следующих ЯП:

* Haskell
* Clojure
* Ruby
* Python
* Java
* C#
* C

### Local checker и Visualizer

Используйте скрипт `local_checker.py` для проверки вашего бота в бою против idle-бота.
После запуска вы увидите результат сражения, также будет сгенерирован визуализатор боя.
Для запуска визуализатора откройте сгенерированный `index.html`

### Пример

#### Java
```
ant -f ./java/build.xml jar
python ./local_checker.py "java -jar ./java/UpgradeBot.jar"
open ./index.html
```

#### Python
```
python ./local_checker.py -m medium python ./python/random_bot.py
open ./index.html
```
