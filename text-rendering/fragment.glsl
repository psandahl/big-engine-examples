#version 330 core

in vec2 vTexCoord;

uniform sampler2D fontAtlas;

out vec4 color;

void main()
{
  //color = vec4(0, 1, 0, 1);
  float alpha = texture(fontAtlas, vTexCoord).a;
  /*if (alpha > 0.5) {
    color = vec4(1, 0, 0, 1);
  } else {
    discard;
  }*/
  color = vec4(1, 0, 0, alpha);
}
