object UnityUpdaterService: TUnityUpdaterService
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'Unity Updater Service'
  AfterInstall = ServiceAfterInstall
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
