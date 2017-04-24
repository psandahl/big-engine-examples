#version 330 core

layout (location = 0) in vec3 position;

uniform mat4 persp;

void main()
{
  gl_Position = persp * vec4(position, 1.0);
}
