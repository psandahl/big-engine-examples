#version 330 core

layout (location = 0) in vec3 position;

void main()
{
  mat4 trans = mat4(vec4(1.0, 0.0, 0.0, -0.5),
                    vec4(0.0, 1.0, 0.0, 0.0),
                    vec4(0.0, 0.0, 1.0, 0.0),
                    vec4(0.0, 0.0, 0.0, 1.0)
                   );
  mat4 scale = mat4(vec4(1.0, 0.0, 0.0, 0.0),
                    vec4(0.0, 1.2, 0.0, 0.0),
                    vec4(0.0, 0.0, 1.0, 0.0),
                    vec4(0.0, 0.0, 0.0, 1.0)
                   );
  gl_Position = scale * transpose(trans) * vec4(position, 1.0);
}
