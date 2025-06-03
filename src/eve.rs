use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SliceSelect {
    Horizontal,
    Vertical,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Alignment {
    Left,
    Right,
    Top,
    Bottom,
    Center,
    Proportional,
    Fill,
    Forward,
    Reverse,
    ForwardFill,
    ReverseFill,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Placement {
    Row,
    Column,
    Overlay,
    Leaf,
}

#[derive(Debug, Clone)]
pub struct LayoutSlice {
    pub alignment: Alignment,
    pub margin: (i32, i32),
    pub suppress: bool,
}

impl Default for LayoutSlice {
    fn default() -> Self {
        Self {
            alignment: Alignment::Left,
            margin: (0, 0),
            suppress: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LayoutAttributes {
    pub slice: [LayoutSlice; 2], // horizontal, vertical
    pub indent: i32,
    pub placement: Placement,
}

impl Default for LayoutAttributes {
    fn default() -> Self {
        Self {
            slice: [LayoutSlice::default(), LayoutSlice::default()],
            indent: 0,
            placement: Placement::Leaf,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PlaceData {
    pub slice: [PlaceSlice; 2],
}

#[derive(Debug, Clone)]
pub struct PlaceSlice {
    pub length: i32,
    pub position: i32,
}

impl Default for PlaceSlice {
    fn default() -> Self {
        Self {
            length: 0,
            position: 0,
        }
    }
}

impl Default for PlaceData {
    fn default() -> Self {
        Self {
            slice: [PlaceSlice::default(), PlaceSlice::default()],
        }
    }
}

pub type GuideSet = Vec<i32>;

pub trait Placeable {
    fn measure(&self, slice: SliceSelect) -> (i32, i32); // min, max
    fn place(&mut self, place_data: &PlaceData);
}

pub struct ViewProxy {
    pub placeable: Box<dyn Placeable>,
    pub visible: bool,
    pub geometry: LayoutAttributes,
    pub place: PlaceData,
    pub space_before: i32,
    pub container_length: [i32; 2],
    pub measured_length: [i32; 2],
    pub container_guide_set: [[GuideSet; 2]; 2], // [slice][forward/reverse]
}

impl std::fmt::Debug for ViewProxy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ViewProxy")
            .field("visible", &self.visible)
            .field("geometry", &self.geometry)
            .field("place", &self.place)
            .field("space_before", &self.space_before)
            .field("container_length", &self.container_length)
            .field("measured_length", &self.measured_length)
            .field("container_guide_set", &self.container_guide_set)
            .field("placeable", &"<trait object>")
            .finish()
    }
}

impl ViewProxy {
    pub fn new(geometry: LayoutAttributes, placeable: Box<dyn Placeable>) -> Self {
        Self {
            placeable,
            visible: true,
            geometry,
            place: PlaceData::default(),
            space_before: 0,
            container_length: [0, 0],
            measured_length: [0, 0],
            container_guide_set: [[Vec::new(), Vec::new()], [Vec::new(), Vec::new()]],
        }
    }

    pub fn calculate(&mut self) {
        for slice in [SliceSelect::Horizontal, SliceSelect::Vertical] {
            let slice_idx = slice as usize;
            let (min, _max) = self.placeable.measure(slice);
            self.container_length[slice_idx] = min;
            self.measured_length[slice_idx] = min;
        }
    }

    pub fn place(&mut self) {
        self.placeable.place(&self.place);
    }
}

pub type ViewProxyRef = Rc<RefCell<ViewProxy>>;
pub type ViewProxyWeakRef = Weak<RefCell<ViewProxy>>;

#[derive(Debug)]
pub struct ProxyNode {
    pub proxy: ViewProxyRef,
    pub parent: Option<ViewProxyWeakRef>,
    pub children: Vec<ViewProxyRef>,
}

impl ProxyNode {
    pub fn new(proxy: ViewProxyRef) -> Self {
        Self {
            proxy,
            parent: None,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EvaluateOptions {
    pub kEvaluateNoPlace: bool,
}

impl Default for EvaluateOptions {
    fn default() -> Self {
        Self {
            kEvaluateNoPlace: false,
        }
    }
}

pub struct Eve {
    root: Option<ViewProxyRef>,
    proxies: HashMap<usize, ProxyNode>,
    next_id: usize,
}

impl Eve {
    pub fn new() -> Self {
        Self {
            root: None,
            proxies: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn evaluate(&mut self, options: EvaluateOptions, width: i32, height: i32) -> (i32, i32) {
        // Calculate all proxies
        self.calculate_all();

        // Adjust and return dimensions
        self.adjust(options, width, height)
    }

    pub fn adjust(&mut self, options: EvaluateOptions, width: i32, height: i32) -> (i32, i32) {
        let root_clone = self.root.clone();
        if let Some(root) = root_clone {
            {
                let mut root_proxy = root.borrow_mut();

                // Set root dimensions
                root_proxy.container_length[SliceSelect::Horizontal as usize] = width;
                root_proxy.container_length[SliceSelect::Vertical as usize] = height;
            }

            // Solve constraints
            self.solve(SliceSelect::Horizontal);
            self.solve(SliceSelect::Vertical);

            // Layout if not disabled
            if !options.kEvaluateNoPlace {
                self.layout(SliceSelect::Horizontal, width);
                self.layout(SliceSelect::Vertical, height);
            }

            let root_proxy = root.borrow();
            (root_proxy.measured_length[0], root_proxy.measured_length[1])
        } else {
            (0, 0)
        }
    }

    pub fn add_placeable(
        &mut self,
        parent_id: Option<usize>,
        initial: LayoutAttributes,
        _is_container_type: bool,
        placeable: Box<dyn Placeable>,
        reverse: bool,
    ) -> usize {
        let id = self.next_id;
        self.next_id += 1;

        let proxy = Rc::new(RefCell::new(ViewProxy::new(initial, placeable)));
        let mut node = ProxyNode::new(proxy.clone());

        if let Some(parent_id) = parent_id {
            if let Some(parent_node) = self.proxies.get_mut(&parent_id) {
                node.parent = Some(Rc::downgrade(&parent_node.proxy));
                if reverse {
                    parent_node.children.insert(0, proxy.clone());
                } else {
                    parent_node.children.push(proxy.clone());
                }
            }
        } else {
            self.root = Some(proxy.clone());
        }

        self.proxies.insert(id, node);
        id
    }

    pub fn set_visible(&mut self, id: usize, visible: bool) {
        if let Some(node) = self.proxies.get(&id) {
            node.proxy.borrow_mut().visible = visible;
        }
    }

    pub fn set_layout_attributes(&mut self, id: usize, geometry: LayoutAttributes) {
        if let Some(node) = self.proxies.get(&id) {
            node.proxy.borrow_mut().geometry = geometry;
        }
    }

    fn calculate_all(&mut self) {
        // Post-order traversal to calculate all proxies
        if let Some(root) = &self.root {
            self.calculate_recursive(root);
        }
    }

    fn calculate_recursive(&self, proxy_ref: &ViewProxyRef) {
        let proxy = proxy_ref.borrow();
        let children: Vec<_> = self.get_children_for_proxy(proxy_ref);
        drop(proxy);

        // Calculate children first (post-order)
        for child in &children {
            self.calculate_recursive(child);
        }

        // Calculate this proxy
        proxy_ref.borrow_mut().calculate();
    }

    fn get_children_for_proxy(&self, proxy_ref: &ViewProxyRef) -> Vec<ViewProxyRef> {
        for node in self.proxies.values() {
            if Rc::ptr_eq(&node.proxy, proxy_ref) {
                return node
                    .children
                    .iter()
                    .filter(|child| child.borrow().visible)
                    .cloned()
                    .collect();
            }
        }
        Vec::new()
    }

    fn solve(&mut self, slice: SliceSelect) {
        // Simplified constraint solving
        // In a full implementation, this would implement the guide-based constraint system
        if let Some(root) = &self.root {
            self.solve_recursive(root, slice);
        }
    }

    fn solve_recursive(&self, proxy_ref: &ViewProxyRef, slice: SliceSelect) {
        let children = self.get_children_for_proxy(proxy_ref);

        // Solve children first
        for child in &children {
            self.solve_recursive(child, slice);
        }

        // Solve this proxy's constraints
        self.solve_proxy_constraints(proxy_ref, &children, slice);
    }

    fn solve_proxy_constraints(
        &self,
        proxy_ref: &ViewProxyRef,
        children: &[ViewProxyRef],
        slice: SliceSelect,
    ) {
        let mut proxy = proxy_ref.borrow_mut();
        let slice_idx = slice as usize;

        // Simple constraint solving - sum children lengths
        let mut total_length = 0;
        for child in children {
            let child_proxy = child.borrow();
            total_length += child_proxy.measured_length[slice_idx];
        }

        proxy.measured_length[slice_idx] = total_length.max(proxy.container_length[slice_idx]);
    }

    fn layout(&mut self, slice: SliceSelect, optional_length: i32) {
        if let Some(root) = &self.root {
            let mut root_proxy = root.borrow_mut();
            root_proxy.place.slice[slice as usize].length = if optional_length > 0 {
                optional_length
            } else {
                root_proxy.container_length[slice as usize]
            };
            drop(root_proxy);

            self.layout_recursive(root, slice);
        }
    }

    fn layout_recursive(&self, proxy_ref: &ViewProxyRef, slice: SliceSelect) {
        let children = self.get_children_for_proxy(proxy_ref);

        // Layout children
        self.layout_children(proxy_ref, &children, slice);

        // Recursively layout children
        for child in &children {
            self.layout_recursive(child, slice);
        }
    }

    fn layout_children(
        &self,
        parent_ref: &ViewProxyRef,
        children: &[ViewProxyRef],
        slice: SliceSelect,
    ) {
        let parent = parent_ref.borrow();
        let slice_idx = slice as usize;
        let available_length = parent.place.slice[slice_idx].length;
        drop(parent);

        if children.is_empty() {
            return;
        }

        // Simple layout - distribute space evenly
        let child_length = available_length / children.len() as i32;
        let mut position = 0;

        for child in children {
            let mut child_proxy = child.borrow_mut();
            child_proxy.place.slice[slice_idx].position = position;
            child_proxy.place.slice[slice_idx].length = child_length;
            position += child_length;
        }
    }
}

impl Default for Eve {
    fn default() -> Self {
        Self::new()
    }
}

// Helper functions for layout attributes
pub fn set_margin(container: &mut LayoutAttributes, x: i32) {
    container.slice[SliceSelect::Horizontal as usize].margin = (x, x);
    container.slice[SliceSelect::Vertical as usize].margin = (x, x);
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestPlaceable {
        min_size: (i32, i32),
        max_size: (i32, i32),
    }

    impl TestPlaceable {
        fn new(min_size: (i32, i32), max_size: (i32, i32)) -> Self {
            Self { min_size, max_size }
        }
    }

    impl Placeable for TestPlaceable {
        fn measure(&self, slice: SliceSelect) -> (i32, i32) {
            match slice {
                SliceSelect::Horizontal => (self.min_size.0, self.max_size.0),
                SliceSelect::Vertical => (self.min_size.1, self.max_size.1),
            }
        }

        fn place(&mut self, _place_data: &PlaceData) {
            // Test implementation - no actual placement
        }
    }

    #[test]
    fn test_eve_creation() {
        let eve = Eve::new();
        assert!(eve.root.is_none());
    }

    #[test]
    fn test_add_placeable() {
        let mut eve = Eve::new();
        let placeable = Box::new(TestPlaceable::new((100, 50), (200, 100)));
        let attributes = LayoutAttributes::default();

        let id = eve.add_placeable(None, attributes, false, placeable, false);
        assert_eq!(id, 0);
        assert!(eve.root.is_some());
    }

    #[test]
    fn test_evaluate() {
        let mut eve = Eve::new();
        let placeable = Box::new(TestPlaceable::new((100, 50), (200, 100)));
        let attributes = LayoutAttributes::default();

        eve.add_placeable(None, attributes, false, placeable, false);
        let (width, height) = eve.evaluate(EvaluateOptions::default(), 300, 200);

        assert!(width >= 0);
        assert!(height >= 0);
    }
}

