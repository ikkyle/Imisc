done <- function(msg='', dev=1){
  pbPost(type='note',
         title='Done!',
         body=msg, 
         recipients=dev)
  beep()
}