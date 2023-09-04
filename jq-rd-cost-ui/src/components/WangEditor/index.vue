<template>
  <div id="WangEditor">
    <Toolbar
      style="border-bottom: 1px solid #ccc"
      :editor="editor"
      :defaultConfig="toolbarConfig"
      :mode="mode"
    />
    <Editor
      style="height: 500px; overflow-y: hidden;"
      v-model="html"
      :defaultConfig="editorConfig"
      :mode="mode"
      @onCreated="onCreated"
    />
  </div>
</template>

<script>

import '@wangeditor/editor/dist/css/style.css'
import { Editor, Toolbar } from '@wangeditor/editor-for-vue'
import { getToken } from '@/utils/auth'
import { uploadEditorImage } from '@/api/common'

export default {
  name: 'WangEditor',
  components: { Editor, Toolbar },

  props: {
    value: {
      type: String,
      default: ''
    },
    placeholder: {
      type: String,
      default: '请输入内容...'
    }
  },
  watch: {
    //初始化赋值
    value(val) {
      console.log(val)
      this.html = val
      // console.log(this.html)
    },
    html(val) {
      this.$emit('update:update', val)
    }
  },

  data() {
    return {
      editor: null,
      html: null,
      toolbarConfig: {},
      editorConfig: {
        placeholder: this.placeholder,
        // autoFocus: false,
        // 所有的菜单配置，都要在 MENU_CONF 属性下
        MENU_CONF: {
          uploadImage: {
            customUpload: this.uploadImg,
            // form-data fieldName，后端接口参数名称，默认值wangeditor-uploaded-image
            fieldName: 'file',
            // 1M，单个文件的最大体积限制，默认为 2M
            maxFileSize: 1 * 1024 * 1024,
            // 最多可上传几个文件，默认为 100
            maxNumberOfFiles: 10,
            // 选择文件时的类型限制，默认为 ['image/*'] 。如不想限制，则设置为 []
            allowedFileTypes: ['image/*'],
            // 15 秒，超时时间，默认为 10 秒
            timeout: 15 * 1000,
            // 自定义上传参数，例如传递验证的 token 等。参数会被添加到 formData 中，一起上传到服务端。
            // meta: {
            //     token: 'xxx',
            //     otherKey: 'yyy'
            // },
            // 将 meta 拼接到 url 参数中，默认 false
            // metaWithUrl: false,
            // 自定义增加 http  header
            headers: {
              Authorization: 'Bearer ' + getToken()
            }
            // 跨域是否传递 cookie ，默认为 false
            // withCredentials: false,
          }
        }
      },
      mode: 'default' // or 'simple'
    }
  },

  methods: {
    onCreated(editor) {
      this.editor = Object.seal(editor) // 一定要用 Object.seal() ，否则会报错
      this.html = this.value
    },

    /** 自定义上传图片到后台，返回url */
    uploadImg(file, insertFn) {
      let formData = new FormData()
      formData.append('file', file)
      // file 即选中的文件
      // 自己实现上传，并得到图片 url alt href
      uploadEditorImage(formData).then(res => {
        if (res.errno == 0) {
          // 最后插入图片
          insertFn(process.env.VUE_APP_BASE_API + res.data.url, file.name, process.env.VUE_APP_BASE_API + res.data.url)
        } else {
        }
      })
    }
  },
  beforeDestroy() {
    const editor = this.editor
    if (editor == null) return
    editor.destroy() // 组件销毁时，及时销毁编辑器
  }
}
</script>

<style>
#WangEditor {
  border: solid #00000024 1px;
}
</style>

