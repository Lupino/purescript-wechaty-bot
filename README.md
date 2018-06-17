# 命令行版的微信

通过命令行微信我们可以做自己想做的事情，如自动聊天（`Chat.purs`），
定时发消息，提醒等等

## 编译

```bash
git clone https://github.com/Lupino/purescript-wechaty-bot.git
npm install
npm run build
```

## 启动

1. 安装定时任务系统 [periodic](https://github.com/Lupino/haskell-periodic).
2. 修改配置文件 `Config.purs` 然后重新编译
3. 启动

```bash
npm run build
periodicd -H tcp://127.0.0.1:5000
node index.js 2> stderr.log
```

4. 打开另外一个终端然后查看 `stderr.log` 消息都会输出到这里.

```bash
tail -f stderr.log
```

5. 用微信扫二维码登录


## 使用

1. 跟好友聊天

```hash
.contact 好友昵称
```

2. 微信群聊天

```bash
.room 微信群名
```

3. 如果出现多个同名的群或者好友用 `.select`

```bash
.select 1
```

4. 跟自己聊天

```bash
.exit
```

5. 只输出某些聊天内容用 `.whitelist`

```bash
.whitelist                # 显示白名单
.whitelist add STRING     # 添加显示白名单
.whitelist remove STRING  # 移除显示白名单
.whitelist clear          # 清空显示白名单
```

6. 延后发消息或者定时间发消息

```bash
.task add STRING          # 添加任务
.task list                # 查看所有任务
.task get INT             # 查看单条任务
.task schedin INT STRING  # 任务多长时间后执行
.task repeat INT STRING   # 任务重复周期
.task del INT             # 删除任务
```

6.1. 选择好友或者群

```bash
.contact STRING
# OR
.room STRING
# OR
.exit                # 给自己发
```

6.2. 添加消息

```bash
.task add 消息内容
```

6.3. 设定消息多久后发送

```bash
.task .schedin 消息ID 时间字符串
```

时间字符串格式: `1Y 2M 3d 4h 5m 6s`

6.4. 隔一段时间重复发送

```bash
.task repeat 消息ID 时间字符串
```

6.5. 删除消息

```bash
.task del 消息ID
```
