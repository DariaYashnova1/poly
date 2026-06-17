## 💅 Как запускать это чудо:

1) VS: `Ctrl+Shift+B` (Сборка -> Сборка решения)

`cmilan.exe` появился в `cmilan\src\cmilan_vs2011\Debug\cmilan.exe`. Этот путь мы везде прописываем потом в `cmd`.

2) Компиляция в `cmd`: `src\cmilan_vs2011\Debug\cmilan.exe test\mybaseloop.mil > mybaseloop.out`
3) Запуск на виртуальной машине `..\vm\bin\milanvm.exe mybaseloop.out`
4) Вывод потока команд `src\cmilan_vs2011\Debug\cmilan.exe test\mybaseloop.mil`
