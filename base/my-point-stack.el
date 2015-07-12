;;; my-pointstack.el --- Keep history of user-specified point moves.


(require 'point-stack)
(global-set-key '[(f5)] 'point-stack-push)
(global-set-key '[(f6)] 'point-stack-pop)
(global-set-key '[(f7)] 'point-stack-forward-stack-pop)

(provide 'my-pointstack)
;;; my-pointstack.el ends here
