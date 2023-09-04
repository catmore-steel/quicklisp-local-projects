<template>
  <div>

    <treeselect v-model="userArray"
                :flat="true"
                :multiple="true"
                :show-count="false"
                :disabled="inputDisabled"
                @input="inputTreeValue"
                @select="selectTreeValue"
                @deselect="deselectTreeValue"
                :disable-branch-nodes="true"
                :options="userOptions"
                :normalizer="userNormalizer"
                :allowSelectingDisabledDescendants="true"
                class="form-control" placeholder="请选择"
    />
  </div>

</template>


<script>
// https://www.vue-treeselect.cn/
import {selectDeptUser} from '@/api/system/user'
import Treeselect from '@riophae/vue-treeselect'
import {isNullOrEmpty} from '@/utils/jq'

export default {
  name: 'JqUserSelect',
  components: {Treeselect},
  data() {
    return {
      userOptions: [],
      userArray: [],
      nowNode: null
    }
  },
  inject: {
    elForm: {
      default: ''
    },
    elFormItem: {
      default: ''
    }
  },
  props: {
    agent: {
      type: [Number, Array]
    },
    checkMany: {
      type: Boolean,
      default: true
    },
    disabled: {
      type: Boolean,
      default: false
    }
  },
  created() {
    this.getUserSelect();
    this.setUserArr(this.agent);
  },
  watch: {
    /** 修改、查看时回显数据 **/
    agent(newVal) {
      this.setUserArr(newVal);
    }
  },
  computed: {
    inputDisabled() {
      return this.disabled || (this.elForm || {}).disabled
    }
  },
  methods: {
    /** 查询人员信息下拉树结构 */
    getUserSelect() {
      selectDeptUser(this.queryParams).then(response => {
        this.userOptions = this.handleTree(response.data, 'id', 'parentId', 'children', 'D0')
      })
    },
    /** 转换人员数据结构 */
    userNormalizer(node) {
      if (node.children && !node.children.length) {
        delete node.children
      }
      return {
        id: node.id,
        label: node.label,
        children: node.children,
        isDisabled: node.blDisabled,
        isNew: node.blNew,
        isDefaultExpanded: node.blDefaultExpanded
      }
    },
    setUserArr(newVal) {
      if (this.isNullOrEmpty(newVal)) {
        this.userArray = []
      } else {
        this.userArray = this.turnString(newVal)
      }
    },
    inputTreeValue(node, instanceId) {
      if (node.length > 0) {
        //当前节点大于0
        //非初始化事件
        if (!isNullOrEmpty(this.nowNode)){
          //切换值事件
          if (!this.checkMany) {
            this.userArray = [this.nowNode.id];
            this.$emit("change", this.turnNum(this.nowNode.id))
            this.$emit('update:agent',this.turnNum(this.nowNode.id))
          } else {
            this.$emit("change", this.turnNum(this.userArray))
            this.$emit('update:agent',this.turnNum(this.userArray))
          }
        }
      } else {
        this.$emit("change", null)
        this.$emit('update:agent',null)
      }
    },
    turnNum(strings) {
      if (Array.isArray(strings)) {
        return strings.map(Number);
      } else {
        return parseInt(strings);
      }
    },
    turnString(numbers) {
      if (Array.isArray(numbers)) {
        return numbers.map(String);
      } else {
        return [String(numbers)];
      }
    },
    selectTreeValue(node) {
      this.nowNode = node;
    },
    deselectTreeValue(node) {
    }

  }
}
</script>

<style scoped lang="scss">

.form-control {
  .el-collapse-item__content {
    overflow: auto;
  }

}

</style>
