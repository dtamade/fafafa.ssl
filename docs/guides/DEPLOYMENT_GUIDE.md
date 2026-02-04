# fafafa.ssl 部署指南

> **版本**: v0.8  
> **最后更新**: 2025-10-24

本指南提供在不同环境中部署 fafafa.ssl 应用程序的最佳实践。

## 目录

- [部署检查清单](#部署检查清单)
- [Windows 部署](#windows-部署)
- [Linux 部署](#linux-部署)
- [Docker 容器化](#docker-容器化)
- [云平台部署](#云平台部署)
- [高可用配置](#高可用配置)
- [监控与日志](#监控与日志)

---

## 部署检查清单

### 编译前检查
- [ ] 使用 Release 模式编译
- [ ] 启用优化选项 (`-O2`, `-O3`)
- [ ] 禁用调试符号（生产环境）
- [ ] 静态链接依赖（可选）
- [ ] 代码签名（Windows）

### 依赖检查
- [ ] OpenSSL 版本 ≥ 1.1.1 或 3.x
- [ ] 系统库完整（libc, pthreads等）
- [ ] CA 证书包可用
- [ ] 所需端口开放

### 安全检查
- [ ] 证书和私钥安全存储
- [ ] 文件权限正确设置
- [ ] 使用强密码套件
- [ ] 启用证书验证
- [ ] 禁用不安全协议（SSLv2/v3, TLS 1.0）

### 配置检查
- [ ] 日志配置正确
- [ ] 超时设置合理
- [ ] 会话缓存配置（如需要）
- [ ] 资源限制设置

---

## Windows 部署

### 编译 Release 版本

```powershell
# 使用 FPC
fpc -O3 -XX -CX -Xs -Fusrc -Fusrc\openssl your_app.pas

# 或使用 Lazarus
lazbuild --build-mode=Release your_app.lpi

# 编译选项说明：
# -O3: 最高优化
# -XX: 智能链接
# -CX: 去除运行时检查
# -Xs: 不使用控制台（GUI应用）
```

### 打包依赖

```powershell
# 1. 创建发布目录
mkdir Release
copy your_app.exe Release\

# 2. 复制 OpenSSL DLL
copy C:\OpenSSL-Win64\bin\libcrypto-3-x64.dll Release\
copy C:\OpenSSL-Win64\bin\libssl-3-x64.dll Release\

# 3. 复制 CA 证书包
copy ca-bundle.crt Release\

# 4. 复制配置文件
copy config.ini Release\

# 5. 创建必要目录
mkdir Release\logs
mkdir Release\certs
```

### 安装服务（可选）

```powershell
# 使用 NSSM (Non-Sucking Service Manager)
choco install nssm

# 安装服务
nssm install MySSLApp C:\MyApp\Release\your_app.exe

# 配置服务
nssm set MySSLApp AppDirectory C:\MyApp\Release
nssm set MySSLApp Description "My SSL Application"
nssm set MySSLApp Start SERVICE_AUTO_START

# 启动服务
nssm start MySSLApp
```

### Windows Defender 防火墙

```powershell
# 添加入站规则
New-NetFirewallRule `
  -DisplayName "MySSLApp HTTPS" `
  -Direction Inbound `
  -Protocol TCP `
  -LocalPort 443 `
  -Action Allow

# 添加应用程序规则
New-NetFirewallRule `
  -DisplayName "MySSLApp" `
  -Direction Inbound `
  -Program "C:\MyApp\Release\your_app.exe" `
  -Action Allow
```

---

## Linux 部署

### 编译 Release 版本

```bash
# 使用 FPC
fpc -O3 -XX -CX -Fusrc -Fusrc/openssl your_app.pas

# 或使用 Lazarus（无头模式）
lazbuild --build-mode=Release your_app.lpi

# 检查依赖
ldd your_app
```

### 系统准备

```bash
# Ubuntu/Debian
sudo apt update
sudo apt install libssl3

# Fedora/RHEL
sudo dnf install openssl-libs

# 创建应用用户
sudo useradd -r -s /bin/false myapp

# 创建目录结构
sudo mkdir -p /opt/myapp/{bin,conf,certs,logs}
sudo chown -R myapp:myapp /opt/myapp
```

### 部署应用

```bash
# 复制文件
sudo cp your_app /opt/myapp/bin/
sudo cp config.ini /opt/myapp/conf/
sudo cp ca-bundle.crt /opt/myapp/certs/

# 设置权限
sudo chmod 755 /opt/myapp/bin/your_app
sudo chmod 600 /opt/myapp/conf/config.ini
sudo chmod 600 /opt/myapp/certs/*

# 私钥应设置为最严格权限
sudo chmod 400 /opt/myapp/certs/server.key
sudo chown myapp:myapp /opt/myapp/certs/server.key
```

### Systemd 服务

创建 `/etc/systemd/system/myapp.service`:

```ini
[Unit]
Description=My SSL Application
After=network.target

[Service]
Type=simple
User=myapp
Group=myapp
WorkingDirectory=/opt/myapp
ExecStart=/opt/myapp/bin/your_app
Restart=on-failure
RestartSec=10s

# 安全强化
PrivateTmp=true
NoNewPrivileges=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/opt/myapp/logs

# 资源限制
LimitNOFILE=65536
LimitNPROC=512

# 日志
StandardOutput=journal
StandardError=journal
SyslogIdentifier=myapp

[Install]
WantedBy=multi-user.target
```

启动服务：

```bash
# 重载 systemd
sudo systemctl daemon-reload

# 启用开机自启
sudo systemctl enable myapp

# 启动服务
sudo systemctl start myapp

# 查看状态
sudo systemctl status myapp

# 查看日志
sudo journalctl -u myapp -f
```

### Nginx 反向代理

`/etc/nginx/sites-available/myapp`:

```nginx
upstream myapp_backend {
    server 127.0.0.1:8443;
    keepalive 32;
}

server {
    listen 443 ssl http2;
    server_name example.com;

    # SSL 配置
    ssl_certificate /opt/myapp/certs/server.crt;
    ssl_certificate_key /opt/myapp/certs/server.key;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;
    ssl_prefer_server_ciphers on;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;

    # 安全头
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;

    # 代理配置
    location / {
        proxy_pass https://myapp_backend;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

启用站点：

```bash
sudo ln -s /etc/nginx/sites-available/myapp /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

---

## Docker 容器化

### Dockerfile

```dockerfile
# 阶段 1: 编译
FROM ubuntu:22.04 AS builder

# 安装编译工具
RUN apt-get update && apt-get install -y \
    fpc \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# 复制源码
WORKDIR /build
COPY src/ ./src/
COPY your_app.pas ./

# 编译
RUN fpc -O3 -XX -CX -Fusrc -Fusrc/openssl your_app.pas

# 阶段 2: 运行时
FROM ubuntu:22.04

# 安装运行时依赖
RUN apt-get update && apt-get install -y \
    libssl3 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -r -s /bin/false myapp

# 复制应用
WORKDIR /app
COPY --from=builder /build/your_app /app/
COPY config.ini /app/

# 创建目录
RUN mkdir -p /app/logs /app/certs \
    && chown -R myapp:myapp /app

# 设置用户
USER myapp

# 健康检查
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD curl -f https://localhost/health || exit 1

# 暴露端口
EXPOSE 8443

# 启动应用
CMD ["/app/your_app"]
```

### Docker Compose

`docker-compose.yml`:

```yaml
version: '3.8'

services:
  myapp:
    build: .
    container_name: myapp
    restart: unless-stopped
    ports:
      - "8443:8443"
    volumes:
      - ./certs:/app/certs:ro
      - ./logs:/app/logs
      - ./config.ini:/app/config.ini:ro
    environment:
      - TZ=Asia/Shanghai
    networks:
      - myapp-net
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 1G
        reservations:
          cpus: '1'
          memory: 512M

  nginx:
    image: nginx:alpine
    container_name: myapp-nginx
    restart: unless-stopped
    ports:
      - "443:443"
      - "80:80"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./certs:/etc/nginx/certs:ro
    depends_on:
      - myapp
    networks:
      - myapp-net

networks:
  myapp-net:
    driver: bridge
```

### 构建和运行

```bash
# 构建镜像
docker-compose build

# 启动服务
docker-compose up -d

# 查看日志
docker-compose logs -f myapp

# 停止服务
docker-compose down

# 重启服务
docker-compose restart myapp
```

---

## 云平台部署

### AWS (Amazon Web Services)

**EC2 实例**:

```bash
# 1. 创建实例（Amazon Linux 2）
aws ec2 run-instances \
  --image-id ami-xxxxx \
  --instance-type t3.small \
  --key-name my-key \
  --security-group-ids sg-xxxxx

# 2. 配置安全组
aws ec2 authorize-security-group-ingress \
  --group-id sg-xxxxx \
  --protocol tcp \
  --port 443 \
  --cidr 0.0.0.0/0

# 3. SSH 连接并部署
ssh -i my-key.pem ec2-user@instance-ip
# 按照 Linux 部署步骤执行
```

**使用 ECS (Elastic Container Service)**:

```yaml
# task-definition.json
{
  "family": "myapp",
  "containerDefinitions": [{
    "name": "myapp",
    "image": "your-repo/myapp:latest",
    "portMappings": [{
      "containerPort": 8443,
      "protocol": "tcp"
    }],
    "environment": [
      {"name": "ENV", "value": "production"}
    ],
    "logConfiguration": {
      "logDriver": "awslogs",
      "options": {
        "awslogs-group": "/ecs/myapp",
        "awslogs-region": "us-east-1",
        "awslogs-stream-prefix": "ecs"
      }
    }
  }]
}
```

### Azure

**使用 Azure App Service**:

```bash
# 创建资源组
az group create --name myapp-rg --location eastus

# 创建 App Service 计划
az appservice plan create \
  --name myapp-plan \
  --resource-group myapp-rg \
  --sku P1V2 \
  --is-linux

# 部署容器
az webapp create \
  --resource-group myapp-rg \
  --plan myapp-plan \
  --name myapp \
  --deployment-container-image-name your-repo/myapp:latest

# 配置 SSL
az webapp config ssl upload \
  --resource-group myapp-rg \
  --name myapp \
  --certificate-file server.pfx \
  --certificate-password <password>
```

### Google Cloud Platform

**使用 Cloud Run**:

```bash
# 构建镜像并推送到 GCR
gcloud builds submit --tag gcr.io/project-id/myapp

# 部署到 Cloud Run
gcloud run deploy myapp \
  --image gcr.io/project-id/myapp \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated \
  --port 8443

# 映射自定义域名
gcloud run domain-mappings create \
  --service myapp \
  --domain example.com
```

---

## 高可用配置

### 负载均衡

**HAProxy 配置**:

```haproxy
global
    maxconn 4096
    tune.ssl.default-dh-param 2048

defaults
    mode http
    timeout connect 5000ms
    timeout client 50000ms
    timeout server 50000ms

frontend https_frontend
    bind *:443 ssl crt /etc/haproxy/certs/server.pem
    default_backend myapp_backend

backend myapp_backend
    balance roundrobin
    option httpchk GET /health
    server app1 192.168.1.101:8443 check ssl verify none
    server app2 192.168.1.102:8443 check ssl verify none backup
```

### 数据库高可用

```pascal
// 主从复制配置
procedure ConfigureDBReplication;
begin
  // 主库
  FDBPrimary := ConnectDB('primary-db.example.com', 5432);
  
  // 从库（只读）
  FDBReplica := ConnectDB('replica-db.example.com', 5432);
  FDBReplica.ReadOnly := True;
  
  // 读写分离
  if IsReadOperation then
    UseDatabase(FDBReplica)
  else
    UseDatabase(FDBPrimary);
end;
```

---

## 监控与日志

### Prometheus 监控

```pascal
// 导出指标
procedure ExportMetrics;
var
  LMetrics: TStringList;
begin
  LMetrics := TStringList.Create;
  try
    // 连接数
    LMetrics.Add(Format('ssl_connections_total %d', [FTotalConnections]));
    
    // 握手时间
    LMetrics.Add(Format('ssl_handshake_duration_ms %f', [FAvgHandshakeTime]));
    
    // 错误数
    LMetrics.Add(Format('ssl_errors_total %d', [FTotalErrors]));
    
    // 导出到 HTTP 端点
    ServeMetrics(LMetrics, 9090);
  finally
    LMetrics.Free;
  end;
end;
```

### 日志配置

```pascal
// 结构化日志
procedure LogEvent(const aLevel: TLogLevel; const aMessage: string; aData: TJSONObject);
var
  LLogEntry: TJSONObject;
begin
  LLogEntry := TJSONObject.Create;
  try
    LLogEntry.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', Now));
    LLogEntry.Add('level', GetLogLevelString(aLevel));
    LLogEntry.Add('message', aMessage);
    LLogEntry.Add('data', aData);
    
    // 写入文件
    AppendToFile('/var/log/myapp/app.json', LLogEntry.AsJSON);
    
    // 发送到日志服务（可选）
    SendToElasticsearch(LLogEntry);
  finally
    LLogEntry.Free;
  end;
end;
```

### ELK Stack 集成

**Filebeat 配置** (`filebeat.yml`):

```yaml
filebeat.inputs:
- type: log
  enabled: true
  paths:
    - /var/log/myapp/*.json
  json.keys_under_root: true
  json.add_error_key: true

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
  index: "myapp-%{+yyyy.MM.dd}"

setup.kibana:
  host: "kibana:5601"
```

---

## 备份与恢复

### 证书备份

```bash
#!/bin/bash
# backup-certs.sh

BACKUP_DIR="/backup/certs"
DATE=$(date +%Y%m%d)

# 创建备份
tar czf "$BACKUP_DIR/certs-$DATE.tar.gz" /opt/myapp/certs/

# 加密备份
gpg --symmetric --cipher-algo AES256 "$BACKUP_DIR/certs-$DATE.tar.gz"

# 上传到 S3
aws s3 cp "$BACKUP_DIR/certs-$DATE.tar.gz.gpg" s3://myapp-backups/

# 清理旧备份（保留30天）
find "$BACKUP_DIR" -name "certs-*.tar.gz*" -mtime +30 -delete
```

### 应用数据备份

```bash
#!/bin/bash
# backup-app.sh

# 停止服务
systemctl stop myapp

# 备份数据库
pg_dump myapp_db > /backup/db-$(date +%Y%m%d).sql

# 备份配置
tar czf /backup/config-$(date +%Y%m%d).tar.gz /opt/myapp/conf/

# 启动服务
systemctl start myapp
```

---

## 安全加固

### 系统级

```bash
# 关闭不必要的服务
systemctl disable cups bluetooth

# 配置 SELinux/AppArmor
setenforce 1

# 自动安全更新
sudo apt install unattended-upgrades
```

### 应用级

```pascal
// 限制连接数
procedure ConfigureLimits;
begin
  SetMaxConnections(1000);
  SetConnectionTimeout(30000);  // 30 秒
  SetMaxRequestSize(10 * 1024 * 1024);  // 10 MB
end;

// 速率限制
procedure RateLimitMiddleware;
begin
  if GetRequestCountPerMinute(ClientIP) > 100 then
    raise ERateLimitExceeded.Create('Too many requests');
end;
```

---

## 部署检查表

部署后验证：

- [ ] 应用能正常启动
- [ ] HTTPS 连接正常
- [ ] 证书验证通过
- [ ] 日志正常记录
- [ ] 监控指标可见
- [ ] 健康检查通过
- [ ] 负载均衡工作
- [ ] 备份任务运行
- [ ] 防火墙规则生效
- [ ] SSL Labs 测试 A+ 级

---

**下一步**: 查看 [SECURITY_GUIDE.md](SECURITY_GUIDE.md) 了解安全最佳实践

