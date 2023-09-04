import axios from 'axios'
import { getToken } from '@/utils/auth'
import store from '@/store'

export function ai(messages, isNewLine) {
  return new Promise((resolve, rej) => {
    axios({
      method: 'post',
      url: store.getters.apiHostPath + '/highCloud/openAi/chatWithAi',
      headers: { 'Authorization': 'Bearer ' + getToken() },
      data: {
        creditCode: localStorage.getItem('creditCode'),
        model: 'gpt-3.5-turbo',
        temperature: 0.9,
        max_tokens: 1000,
        top_p: 1,
        frequency_penalty: 0.0,
        presence_penalty: 0.6,
        messages
      }
    }).then(res => {
      if (res.data && res.data.code !== 200) {
        res?.data?.msg ? rej(res.data.msg) : rej('未知错误，请联系管理员！')
      } else {
        if (isNewLine) {
          resolve(res.data.data[0])
        } else {
          res.data.data[0].message[0].content = res.data.data[0].message[0].content.replaceAll(`\n\n`, '\n').replace(`"content":"\n`, `"content":"`)
          resolve(res.data.data[0])
        }
      }
    })
  })
}
