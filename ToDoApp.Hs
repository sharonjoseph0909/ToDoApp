{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List
import System.Console.CmdArgs
import Text.Regex
import System.IO
import System.Directory
import Text.Printf

-----------------------------------------------------
-- Data Types
-----------------------------------------------------
data Task = Task { 
  description :: String,
  deadline :: String, -- (MM/DD/YYYY)
  priority :: Int, -- contains high, normal, and low priority
  completed :: Bool
} deriving (Show) -- converts data into a readable String and prints


-- declaring User with name as String, and tasks as [Task]
data User = User {
  name :: String,
  tasks :: [Task] -- tasks has type that uses a list of Tasks
} deriving (Show)


data Appbase = Appbase {
  users :: [User] -- users has type that uses a list of users
} deriving (Show)
-----------------------------------------------------------
-- JSON instances allow haskell -> JSON and JSON -> haskell
-----------------------------------------------------------
instance ToJSON Task where
  toJSON (Task desc deadli priorit comp) =
      object [ "description" .= desc
      , "deadline" .= deadli
      , "priority" .= priorit
      , "completed" .= comp
      ]

-- do notation style
-- means take the JSON object and call it v
-- then use v to obtain the values below (desc, deadli, priorit, etc.)
instance FromJSON Task where
  parseJSON (Object v) = do
      desc <- v .: "description"
      dead <- v .: "deadline"
      pri <- v .: "priority"
      comp <- v .: "completed"
      return (Task desc dead pri comp)


instance ToJSON User where
  toJSON (User uname utasks) =
      object [ "name" .= uname
      , "tasks" .= utasks
      ]


instance FromJSON User where
  parseJSON (Object v) = do
      uname <- v .: "name"
      utasks <- v .: "tasks"
      return (User uname utasks)


instance ToJSON Appbase where
  toJSON (Appbase us) =
      object [ "users" .= us ]


instance FromJSON Appbase where
  parseJSON (Object v) = do
      us <- v .: "users"
      return (Appbase us)
-------------------------------------------------
-- Executable Functions
-------------------------------------------------
addUser :: Appbase -> String -> IO Appbase
addUser (Appbase users) newName =
  case find (\(User name _) -> name == newName) users of
	Just _ -> do
	  putStrLn "User already exists"
	  return (Appbase users)
	Nothing -> do
	  confirm <- confirm
	  if confirm
		then return (Appbase (users ++ [User newName[]]))
		else return (Appbase users)

--helper functions
findUser :: [User] -> String -> Maybe User
findUser users name = 
  find (\(User n _) -> n == name) users
--
isEmpty :: String -> Bool
isEmpty s = all (== ' ') s || s == ""
--
markTaskCompleted :: Appbase -> String -> String -> Appbase
markTaskCompleted (Appbase users) userName taskDescription =
  Appbase (map markTaskCompletedForUser users)
  where
    markTaskCompletedForUser user@(User name tasks)
      | name == userName = User name (map markTaskCompletedForTask tasks)
      | otherwise = user

    markTaskCompletedForTask task@(Task desc deadli priorit comp)
      | desc == taskDescription && not comp = Task desc deadli priorit True -- if the task description matches the taskDesc parameter, mark it as completed (set completed to True)
      | otherwise = task -- if the task description does not match, return the task unchanged

-- delete a task from a user's task list
deleteTask :: Appbase -> String -> String -> Appbase
deleteTask (Appbase users) userName taskDescription =
  Appbase (map deleteTaskForUser users)
  where
   deleteTaskForUser user@(User name tasks)
      | name == userName = User name (deleteTaskFromList tasks)
      | otherwise = User name tasks

   deleteTaskFromList [] = [] -- if the task list is empty, return an empty list
   
   deleteTaskFromList (task@(Task desc _ _ _):rest)
      | desc == taskDescription = rest -- if the task description matches the taskDesc parameter, delete it by returning the rest of the task list (excluding the matched task)
      | otherwise = task : deleteTaskFromList rest -- if the task description does not match, keep the task and contin

--------------------------------------------------
-- Display Functions
--------------------------------------------------
-- I also need to create a function that will display a user's task list
displayUserTasks :: Appbase -> String -> IO ()
displayUserTasks (Appbase users) userName =
  case find (\(User name _) -> name == userName) users of
    Just (User _ tasks) -> do
      let incomplete = filter (\t -> not (completed t)) tasks
      let complete   = filter completed tasks

      let sortedIncomplete = sortTasks incomplete
      let sortedComplete = sortTasks complete

      putStrLn "Incomplete Tasks:"
      mapM_ (\(index, task) -> printTask index task) (zip [1..] sortedIncomplete) -- if the user is found, print each task in their task list
      
      putStrLn "\nCompleted Tasks:"
      mapM_ (\(index, task) -> printTask index task) (zip [1..] sortedComplete)
    
    Nothing ->
	  putStrLn "User not found" -- if the user is not found, print an error message
--
  where
	printTask :: Int -> Task -> IO()
	printTask index (Task desc deadli priorit comp) =
		let status :: String
		    status = if comp then "Completed" else "Not Completed"
		in printf "[%d]\nDescription: %s\nDeadline: %s\nPriority: %d\nStatus: %s\n\n"
		     index desc deadli priorit status
--
displayAllUsersAndTasks :: Appbase -> IO ()
displayAllUsersAndTasks (Appbase users) = mapM_ displayUser users

  where 
	displayUser (User name tasks) = do
		putStrLn ("User:" ++ name)
		mapM_ printTask tasks

	printTask (Task desc deadli priorit comp) =
		let status = if comp then "Completed" else "Not Completed"
		in putStrLn ("Description: " ++ desc ++
					"\nDeadline: " ++ deadli ++
					"\nPriority: " ++ show priorit ++
					"\nStatus: " ++ status ++ "\n")

-- Displays all task deadlines
displayTasksByDeadline :: Appbase -> String -> IO ()
displayTasksByDeadline (Appbase users) deadline = do
	let tasksDue =
			concatMap (\(User _ tasks) ->
				filter (\(Task _ deadli _ _) -> deadli == deadline) tasks
			) users

	if null tasksDue
		then putStrLn "No tasks due on this date"
		else mapM_ printTask tasksDue

  where
	printTask (Task desc deadli priorit comp) =
		let status =
			    if comp then "Completed" else "Not Completed"
		in putStrLn (
			"Description: " ++ desc ++
			"\nDeadline: " ++ deadli ++
			"\nPriority: " ++ show priorit ++
			"\nStatus: " ++ status ++ "\n"
		)

displayTasksByPriority :: Appbase -> Int -> IO()
displayTasksByPriority (Appbase users) priority = do
	let tasksByPriority =
        	concatMap (\(User _ tasks) ->
				filter (\(Task _ _ p _) -> p == priority) tasks
			) users -- find all tasks that have a priority matching the specified priority
	
	if null tasksByPriority
		then putStrLn "No tasks with this priority" -- if no tasks have the specified priority, print a message
		else mapM_ printTask tasksByPriority -- if there are tasks with the specified priority, print each task
        
  where
    printTask (Task desc deadli priorit comp) =
       let status = if comp then "Completed" else "Not Completed"
	   in putStrLn (
			"Description: " ++ desc ++
			"\nDeadline: " ++ deadli ++
			"\nPriority: " ++ show priorit ++
			"\nStatus: " ++ status ++ "\n"
	    ) -- print the task details in a formatted way 

-- Displays all tasks that are completed
displayCompletedTasks :: Appbase -> IO ()
displayCompletedTasks (Appbase users) = do
	let completedTasks =
			concatMap (\(User _ tasks) ->
              	filter (\(Task _ _ _ comp) -> comp) tasks
			) users -- find all tasks that are marked as completed
			
	if null completedTasks
		then putStrLn "No completed tasks" -- if no tasks are completed, print a message
		else mapM_ printTask completedTasks -- if there are completed tasks, print each
       
  where
    printTask (Task desc deadli priorit comp) =
	   let status = "Completed"
	   in putStrLn (
			"Description: " ++ desc ++
			"\nDeadline: " ++ deadli ++
			"\nPriority: " ++ show priorit ++
			"\nStatus: " ++ status ++ "\n"
	    ) -- print the task details in a formatted way 
-----------------------------------------------
-- Save to AppBase 
-----------------------------------------------
saveAppbase :: Appbase -> FilePath -> IO()
saveAppbase appbase filePath = do
  let jsonData = encode appbase -- convert the Appbase data back into JSON format (ByteString)
  B.writeFile filePath jsonData -- write the JSON data back to the file

load :: IO Appbase
load = do
  exists <- doesFileExist "appbase.json" 

  if not exists
	then do 
	  putStrLn "No database found. Creating a new one..."
	  let empty = Appbase []
	  saveAppbase empty "appbase.json"
	  return empty
	else do
	  c <- B.readFile "appbase.json"
	  case decode c of
		Just app -> return app
		Nothing -> return (Appbase [])
--------------------------------------------------
-- Sorting (Priority Ranking)
-------------------------------------------------
priorityRank :: Int -> Int
priorityRank p = p

sortTasks :: [Task] -> [Task]
sortTasks = sortBy compareTasks
  where
    compareTasks a b =
        compare (completed a) (completed b) <>
        compare (deadline a) (deadline b) <>
        compare (priority b) (priority a) <>
        compare (description a) (description b)
-------------------------------------------------
validPriority :: Int -> Bool
validPriority p = p == 0 || p == 1 || p == 2

validDate :: String -> Bool
validDate date = matchRegex (mkRegex "^(0[1-9]|1[0-2])/(0[1-9]|[12][0-9]|3[01])/[0-9]{4}$") date /= Nothing

confirm :: IO Bool
confirm = do
  putStrLn "Is this correct? Yes or No?"
  ans <- getLine
  return (ans == "Yes" || ans == "yes")

-----------------------------------------------------------
-- Validation Check
-----------------------------------------------------------
addUserIO :: Appbase -> String -> IO Appbase
addUserIO (Appbase users) user = do
  if isEmpty user
    then do
      putStrLn "Username cannot be empty!"
      return (Appbase users)
    else case find (\(User n _) -> n == user) users of
      Just _ -> do
        putStrLn "User already exists."
        return (Appbase users)
      
      Nothing -> do
        ok <- confirm
        if ok
          then do
            putStrLn "User added successfully."
            return (Appbase (users ++ [User user []]))
          else return (Appbase users)

addTaskIO :: Appbase -> String -> Task -> IO Appbase
addTaskIO app@(Appbase users) userName newTask = do

  if isEmpty userName || isEmpty (description newTask)
    then do
      putStrLn "User or task description can not be empty."
      return (Appbase users)
    else
      case find (\(User name _) -> name == userName) users of
    
      Nothing -> do
        putStrLn "Attempting to add a task to a user that does not exist..."
        return (Appbase users)

      Just (User _ tasks) -> do
        if any (\t -> description t == description newTask) tasks
          then do 
            putStrLn "Task already exists."
            return (Appbase users)

          else do

            ok <- confirm
            if not ok
              then do
                putStrLn "Invalid option."
                return app
              else do
                let updatedUsers = map addTaskToUser users
                putStrLn "Task successfully added!"
                return (Appbase updatedUsers)
  where
    addTaskToUser user@(User name tasks)
      | name == userName = User name (tasks ++ [newTask])
      | otherwise = user
--

completeIO app@(Appbase users) userName taskDesc = do
  case findUser users userName of 
    Nothing -> do
      putStrLn "Attempting to mark a task for a user not found..."
      return app

    Just (User _ tasks) ->
      case find (\t -> description t == taskDesc) tasks of
        Nothing -> do
          putStrLn "Task not found."
          return app

        Just t ->
          if completed t
            then do
              putStrLn "Task already completed!"
              return app
            else do
              ok <- confirm
              if not ok
                then do
                  putStrLn "Invalid option."
                  return app
                else return (markTaskCompleted app userName taskDesc)

deleteIO app@(Appbase users) userName taskDesc = do
  case findUser users userName of
    Nothing -> do
      putStrLn "Attempting to delete a task for a user not found..."
      return app

    Just (User _ tasks) ->
      case find (\t -> description t == taskDesc) tasks of
        Nothing -> do
          putStrLn "Task not found!"
          return app

        Just _ -> do
          ok <- confirm
          if not ok
            then do
              putStrLn "Invalid option."
              return app
            else return (deleteTask app userName taskDesc)
--------------------------------------------------
-- Reading JSON file
--------------------------------------------------
readAppbase :: FilePath -> IO Appbase
readAppbase filePath = do
  fileContent <- B.readFile filePath-- read the JSON file as a ByteString
  case decode fileContent of
      Just appbase -> return appbase  -- decode (convert) the ByteString into an Appbase data type (Appbase, User, Task)
      Nothing -> return (Appbase [])-- if decoding fails, throw an error
-----------------------------------------------------------
-- Command Line Arguments
-----------------------------------------------------------
data Command
  = AddUser { user :: String }
  | AddTask
      { user :: String
      , desc :: String
      , taskDeadline :: String
      , taskPriority :: Int
      }
  | List { user :: String }
  | Complete { user :: String, task :: String }
  | Delete { user :: String, task :: String }
  deriving (Show, Data, Typeable)

mode = modes
  [ AddUser { user = def &= help "Add user" }

  , AddTask { user = def &= help "Add Task"
            , desc = def &= help "Description"
            , taskDeadline = def &= help "MM/DD/YYYY"
            , taskPriority = def &= help "0 = Low, 1 = Normal, 2 = High" }
  , List { user = def &= help "List Tasks"}
  , Complete { user = def &= help "User", task = def &= help "Mark Completed" }
  , Delete { user = def &= help "User", task = def &= help "Delete Task"}
  ]
  &= program "todo"
  &= help "Run with -? for help"
  &= summary
  "Help Message: ToDo Haskell App\n\
  \Examples:\n\
  \  ./todo adduser --user=\"Johnny\"\n\
  \  ./todo addtask --user=\"Johnny\" --desc=\"Study\" --taskdeadline=\"04/30/2026\" --taskpriority=2\n\
  \  ./todo list --user=\"Johnny\"\n\
  \  ./todo complete --user=\"Johnny\" --task=\"Study\"\n\
  \  ./todo delete --user=\"Johnny\" --task=\"Study\""
---------------------------------------------------
-- Main Function
---------------------------------------------------
main :: IO()
main = do
    fileExists <- doesFileExist "appbase.json"

    if not fileExists
        then writeFile "appbase.json" "{\"users\":[]}"
        else return ()

    cmd <- cmdArgs mode 

    appbase <- readAppbase "appbase.json" -- read the Appbase data from the JSON file

    case cmd of

        AddUser user -> 
          if isEmpty user
            then putStrLn "User cannot be empty"
            else do
              newApp <- addUserIO appbase user
              saveAppbase newApp "appbase.json"

        AddTask user desc taskDeadline taskPriority -> do
            if not (validDate taskDeadline)
                then do
                    putStrLn "Invalid date format. Use MM/DD/YYYY"
                    return ()
                else if not (validPriority taskPriority)
                    then do
                        putStrLn "Invalid priority. Use 0 (Low), 1 (Normal), 2 (High)"
                        return ()
                    else do
                        let task = Task desc taskDeadline taskPriority False
                        newApp <- addTaskIO appbase user task
                        saveAppbase newApp "appbase.json"

        List user ->
            displayUserTasks appbase user

        Complete user task -> do
            newApp <- completeIO appbase user task
            saveAppbase newApp "appbase.json"

        Delete user task -> do
            newApp <- deleteIO appbase user task
            saveAppbase newApp "appbase.json"