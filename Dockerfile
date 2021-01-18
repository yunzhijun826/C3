# -- Dockerfile --
# 这个文件负责构建包含你的程序的 Docker 容器

# 使用 Java 12
FROM openjdk:12-alpine
# 向容器内复制文件
COPY ./* /app/
# 编译程序
WORKDIR /app/
RUN javac -d ./output ./c0test.java
# 将当前目录设为输出目录
WORKDIR /app/output
