---
layout: page
title: Триггер
---
# {{ page.title }}

Ну вот, базовые элементы мы описали. Пришло время настоящей логики.

Триггер (RS-триггер) - элементарное запоминающее устройство. Сначала мы соберем его из логических элементов, а потом оптимизируем по площади, чтобы занимал как можно меньше места.

RS-триггер переключает выходной сигнал по выбору одного из входных и сохраняет его, пока оба входных сигнала отсутствуют. Вариант с одновременным включением обоих входных является запрещенным.
Почему запрещенным - попробуйте сами. Поставьте на паузу, включите оба входа, снимите с паузы.

```layout
:+ red
:o yellow
:O blue
:g green :* green
:y yellow
+-------------------------------------+
|                                     |
| oooo                          oo    |
|                              o  o   |
|  ooo                         o  o   |
| o      OO    OO              o oo   |
|  oo  ggO ooooO oo             oo o  |
|    o   OO    OO OO  OO              |
| ooo             O ooO ooooooooyyy   |
|              OO OO  OO   o          |
|            ooO oo        o          |
|            o OO          o          |
|            o             o   oooo   |
|            ooooooooooooo o          |
|                        o o    oo    |
|            oooooooooooo oo   o  o   |
|            o           o     o  o   |
|            o OO        o     o oo   |
| oooo       ooO oo      o      oo o  |
|              OO OO  OO o            |
| ooo             O ooO ooooooooo     |
| o  o   OO    OO OO  OO              |
| ooo  *gO ooooO oo                   |
| o o    OO    OO                     |
| o  o                                |
|                                     |
+-------------------------------------+
```
{:animated="auto"}
