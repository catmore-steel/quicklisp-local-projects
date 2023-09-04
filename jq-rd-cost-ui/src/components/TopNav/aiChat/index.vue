<template>
  <div :class="['ai_chat',isMini?'mini':'small']">
    <div v-show="!isMini">
      <div class="head_opt">
        你好
        <!--        <i class="el-icon-more" @click="showChoose = !showChoose"></i>-->
        <i class="el-icon-minus" @click="minimize"></i>
      </div>
      <div ref="contentRef" class="content">
        <div v-for="(item,index) in messages" :key="index"
             :style="{justifyContent:styleInfo[item.role].jus}"
             class="msg_wrap"
        >
          <img :src="styleInfo[item.role].img" :style="{order:styleInfo[item.role].order}"/>
          <pre :style="{order:item.role == 'assistant' ? 2 : 1,backgroundColor: styleInfo[item.role].bgColor}"
               class="text" v-html="item.content">
          </pre>
        </div>
      </div>
      <div class="btn_opt">
        <input v-model="question" @keyup.enter="sendMessage"/>
        <button :disabled="loading" @click="sendMessage">发送</button>
      </div>
      <div v-if="showChoose" class="choose_model">
        <div class="d4"></div>
        <div :style="{color:Omodel=='text-curie-001' ? '#409eff' : '#fff'　}" class="model" one
             @click="handleChoose(1)"
        ><i
          class="el-icon-s-promotion"
        ></i>text-curie-001
        </div>
        <div :style="{color:Omodel=='text-davinci-003' ? '#409eff' : '#fff'}" class="model" two
             @click="handleChoose(2)"
        ><i
          class="el-icon-s-promotion"
        ></i>text-davinci-003
        </div>
      </div>
    </div>
    <div v-show="isMini" @click="minimize">
      <img src="./1.png"/>
    </div>

  </div>
</template>

<script>
import { ai } from '@/api/ai/ai'

export default {
  name: 'aiChat',
  data() {
    return {
      loading: false,
      showChoose: false,
      model: 'gpt-3.5-turbo-0301',// 当前的对话模型
      isMini: true,  // 最小化
      question: '',
      messages: [
        {role: "system", content: "你好 ~"}
      ],
      styleInfo: {
        assistant: {
          jus: 'start',
          img: require('./1.png'),
          order: 1,
          bgColor: '#fff'
        },
        system: {
          jus: 'start',
          img: require('./1.png'),
          order: 1,
          bgColor: '#fff'
        },
        user: {
          jus: 'end',
          img: '/user.png',
          order: 2,
          bgColor: '#95eb6b'
        }
      }
    }
  },
  mounted() {
  },
  methods: {
    // 选择切换模型
    handleChoose(i) {
      this.Omodel = i == 1 ? 'text-curie-001' : 'text-davinci-003'
      setTimeout(() => {
        this.showChoose = false
      }, 500)
    },
    sendMessage() {
      if( this.loading ) return
      this.loading = true
      this.messages.push({role: "user", content: this.question})
      this.runAi()
    },
    async runAi() {
      ai(this.messages,true).then(res => {
        this.messages.push(res.message[0])
        this.question = ''
        this.loading = false
      }).catch(e => {
        this.messages.push({role: "system", content: e})
        this.question = ''
        this.loading = false
      })
    },
    // 最小化 最大化
    minimize() {
      this.isMini = !this.isMini
    }
  }
}
</script>

<style lang="scss">
.mini {
  width: 40px;
  height: 40px;
  bottom: 200px;
  right: 0px;
  cursor: pointer;

  img {
    padding: 10px;
    width: 100%;
  }
}

.mini:hover {
  img {
    transition: transform 1s;
    transform: rotate(360deg);
  }
}

.small {
  width: 27%;
  height: 600px;
  top: 100px;
  left: 200px;
  animation: anim 4s linear;
}

.ai_chat {
  position: fixed;
  background-color: #ededed;
  border-radius: 8px;
  overflow: hidden;
  box-shadow: 10px 10px 20px #ccc;
  z-index: 56;
  font-size: 14px;
  transition: all .3s linear;

  .head_opt {
    text-align: center;
    line-height: 45px;
    height: 45px;
    width: 100%;
    background-color: #ededed;
    border-bottom: 1px solid #ebebeb;
    padding: 0 5%;

    i {
      font-size: 16px;
      margin-right: 10px;
      font-weight: 600;
      line-height: 45px;
      float: right;
      cursor: pointer;
    }
  }

  .content {
    overflow-y: scroll;
    height: 510px;
    padding: 10px;
    color: #333;
    font-size: 14px;
    display: flex;
    flex-direction: column;

    .msg_wrap {
      margin-bottom: 15px;
      display: flex;
      width: 100%;

      img {
        border-radius: 4px;
        width: 30px;
        height: 30px;
      }

      .text {
        cursor: pointer;
        margin: 0 10px;
        max-width: 80%;
        word-break: break-all;
        border-radius: 4px;
        //background-color: #95eb6b;
        //border: 1px solid #b6e6a0;
        padding: 6px 8px;
        white-space: pre-wrap; /*css-3*/
        white-space: -moz-pre-wrap; /*Mozilla,since1999*/
        white-space: -pre-wrap; /*Opera4-6*/
        white-space: -o-pre-wrap; /*Opera7*/
        word-wrap: break-word; /*InternetExplorer5.5+*/
      }
    }
  }

  .content::-webkit-scrollbar {
    width: 0 !important;
    overflow: -moz-scrollbars-none;
    -ms-overflow-style: none;
  }


  .btn_opt {
    position: absolute;
    height: 45px;
    display: flex;
    align-items: center;
    bottom: 0;
    left: 0;
    width: 100%;
    background-color: #f7f7f7;
    padding: 0 5%;

    input, button {
      height: 28px;
      border: none;
      border-radius: 3px;
      outline: none;
    }

    input {
      width: 75%;
    }

    button {
      font-size: 12px;
      color: #fff;
      background-color: #06c160;
      margin-left: 5%;
      width: 20%;
      cursor: pointer;
    }

    button[disabled] {
      background: #b3e09d !important;
    }
  }
}

.choose_model {
  font-size: 18px;
  position: absolute;
  height: 90px;
  top: 45px;
  right: 8px;
  width: 180px;
  z-index: 66;

  .d4 {
    position: absolute;
    top: -20px;
    right: 16px;
    width: 0;
    height: 0;
    border-width: 10px;
    border-style: solid;
    border-color: transparent #4c4c4c transparent transparent;
    transform: rotate(90deg); /*顺时针旋转90°*/
  }

  .model {
    background-color: #4c4c4c;
    cursor: pointer;
    height: 45px;
    line-height: 45px;
    border-bottom: 1px solid #888;

    i {
      font-size: 20px;
      padding: 5px;
    }
  }

  .model[one] {
    border-top-right-radius: 6px;
    border-top-left-radius: 6px;
  }

  .model[two] {
    border-bottom-right-radius: 6px;
    border-bottom-left-radius: 6px;
  }

  .model:hover {
    background-color: #444;
  }
}

</style>
