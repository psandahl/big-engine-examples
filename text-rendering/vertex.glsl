#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoord;

out vec2 vTexCoord;

const float scaleFactor = 1.0 / 5.0;
const float aspectRatio = 1024.0 / 768.0;

void main()
{
  mat4 trans = mat4(vec4(1.0, 0.0, 0.0, -1.0),
                    vec4(0.0, 1.0, 0.0, 0.0),
                    vec4(0.0, 0.0, 1.0, 0.0),
                    vec4(0.0, 0.0, 0.0, 1.0)
                   );
  mat4 scale = mat4(vec4(scaleFactor, 0.0, 0.0, 0.0),
                    vec4(0.0, scaleFactor * aspectRatio, 0.0, 0.0),
                    vec4(0.0, 0.0, scaleFactor, 0.0),
                    vec4(0.0, 0.0, 0.0, 1.0)
                   );
  vTexCoord = texCoord;
  gl_Position = transpose(trans) * scale * vec4(position, 1.0);
}
