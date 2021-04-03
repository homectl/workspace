#version 330
 out float vf0;
 out float vf1;
in vec4 in0;
in vec2 in1;
void main() {
vec2 t0 = in1;
vf0 = t0.x;
vf1 = t0.y;
vec4 t1 = in0;
gl_Position = vec4(t1.x,t1.y,t1.z,t1.w);
}
