<template>
  <treeselect v-model="agentUserArray"
              :flat="true"
              :multiple="true"
              :show-count="true"
              @input="inputTreeValue"
              @select="selectTreeValue"
              @deselect="deselectTreeValue"
              :disable-branch-nodes="true"
              :options="agentUserOptions"
              :normalizer="userNormalizer"
              :allowSelectingDisabledDescendants="true"
              class="form-control" placeholder="请选择"
  />
</template>

<script>
import {selectDeptUser} from "@/api/system/user";
import Treeselect from "@riophae/vue-treeselect";
import {isNullOrEmpty} from "@/utils/jq";

export default {
  name: "JqAgentUserSelect",
  components: {Treeselect},
  data() {
    return {
      agentUserOptions: [],
      agentUserArray: [],
      queryParams: {
        isAgent: 'yes',
      },
    }
  },
  props: {
    value: {
      type: [Number, Array]
    },
    checkMany: {
      type: Boolean,
      default: true,
    },
  },
  created() {
    //获取人员信息
    this.getAgentUserSelect();

  },
  watch: {
    /** 修改、查看时回显数据 **/
    value(newVal, oldVal) {
      if (this.isNullOrEmpty(newVal)) {
        this.agentUserArray = [];
      } else {
        if (this.checkMany == true) {
          this.agentUserArray = newVal;
        } else {
          this.agentUserArray = [];
          this.agentUserArray.push(newVal);
        }
      }
    }
  },
  methods: {
    /** 查询代理人员信息下拉树结构 */
    getAgentUserSelect() {
      selectDeptUser(this.queryParams).then(response => {
        // console.log(response)
        this.agentUserOptions = this.handleTree(response.data, "id", "parentId");
        // console.log("---------" + this.value);
        if (this.checkMany == true) { //多选
          this.agentUserArray = this.value;
        } else {
          if (!isNullOrEmpty(this.value)) {
            this.agentUserArray.push(this.value);
          }
        }
      });
    },
    /** 转换人员数据结构 */
    userNormalizer(node) {
      if (node.children && !node.children.length) {
        delete node.children;
      }
      return {
        id: node.id,
        label: node.label,
        children: node.children,
        isDefaultExpanded: node.blDefaultExpanded
      };
    },
    /** 人员设为单选 **/
    inputTreeValue(nodes) {
      if (this.checkMany == false) {//单选
        const len = nodes.length;
        const lastNode = nodes[len - 1];
        this.agentUserArray = lastNode ? [lastNode] : [];
        this.$emit('update:value', lastNode)
      } else {
        this.$emit('update:value', nodes)
      }
    },
    /** 选中 **/
    selectTreeValue(node) {
      if (this.checkMany == false) { //单选
        let nodeId = node.id;
        this.$emit('change', nodeId);
      } else {
        let arr = new Array();
        if (!this.isNullOrEmpty(this.agentUserArray)) {
          arr = this.agentUserArray;
        }
        arr.push(node.id);
        this.$emit('change', arr);
      }
    },
    /** 取消选中 **/
    deselectTreeValue(node) {
      if (this.checkMany == false) { //单选
        //单选取消不需要返回任何值进行处理
      } else {
        let arr = Array.from(this.agentUserArray);
        const index = arr.indexOf(node.id);
        if (index > -1) {
          arr.splice(index, 1);
        }
        this.$emit('change', arr);
      }
    }
  }
}
</script>

<style scoped>

</style>
