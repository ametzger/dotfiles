(cond
 ((find-font (font-spec :name "Input"))
  (set-frame-font "Input-12"))
 ((find-font (font-spec :name "Input Mono"))
  (set-frame-font "Input Mono-12"))
 ((find-font (font-spec :name "Monaco"))
  (set-frame-font "Monaco-12"))
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas-12")))
