import Z.Console
import Z.Random

namespace DefaultServices

  def live: Environment (Random × Console) := 
    (Environment.of Console.consoleLive).add Random.randomLive

end DefaultServices

