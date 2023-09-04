// 自定义指令 v-copy
import Clipboard from 'clipboard'
import { Message } from 'element-ui'

export default {
  inserted(el) {
    el.style.color = '#1890ff'
    let vDom = document.createElement('i')
    vDom.classList.add('el-icon-document-copy')
    el.appendChild(vDom)
    vDom.addEventListener('click', (e) => {
      e.stopPropagation()
      copyNodeValue(e, el.innerText.trim())
    })
  }
}

function copyNodeValue(e, text) {
  const clipboard = new Clipboard(e.target, { text: () => text })
  clipboard.on('success', e => {
    Message({
      message: '复制成功!',
      duration: 3000,
      type: 'success'
    })
    // 释放内存
    clipboard.off('error')
    clipboard.off('success')
    clipboard.destroy()
  })
  // err
  clipboard.on('error', e => {
    Message({
      message: '复制失败!',
      duration: 3000,
      type: 'error'
    })
    clipboard.off('error')
    clipboard.off('success')
    clipboard.destroy()
  })
  clipboard.onClick(e)
}
