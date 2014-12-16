done <- function(msg='', dev=1){
  pbPost(type='note',
         title='Done!',
         body=msg, 
         deviceind=dev)
  beep()
}