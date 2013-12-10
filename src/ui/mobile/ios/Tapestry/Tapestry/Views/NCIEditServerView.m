//
//  NCIEditServerView.m
//  Tapestry
//
//  Created by Ira on 11/18/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIEditServerView.h"
#import "NCIWebSocketConnector.h"

@interface NCIEditServerView()<UITextFieldDelegate, UITableViewDataSource, UITableViewDelegate, UIGestureRecognizerDelegate> {
    UITextField *serverUrlEdit;
    UIButton *clearBtn;
    UIButton *goBtn;
    UITableView *bookmarksTable;
}

@end

static int editServerInputHeigth = 40;
static float iconDim = 40;

static float bookmarkRowHeigth = 44;
static int bookmarkVisibleRowsCount = 7;

static float leftIndent = 10;
static float topIndent = 10;
static float rightIndent = 80;

@implementation NCIEditServerView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        
        bookmarksTable = [[UITableView alloc] initWithFrame:CGRectMake(leftIndent,
                                                                       editServerInputHeigth,
                                                                       self.bounds.size.width - rightIndent,
                                                                       0)];
        bookmarksTable.dataSource = self;
        bookmarksTable.delegate = self;
        [self addSubview:bookmarksTable];
        
        serverUrlEdit = [[UITextField alloc] initWithFrame:CGRectZero];
        serverUrlEdit.backgroundColor = [UIColor colorWithWhite:0.99 alpha:1];
        serverUrlEdit.autocorrectionType = UITextAutocorrectionTypeNo;
        serverUrlEdit.autocapitalizationType = UITextAutocapitalizationTypeNone;
        serverUrlEdit.layer.borderColor = [UIColor grayColor].CGColor;
        serverUrlEdit.layer.borderWidth = 0.2;
        serverUrlEdit.placeholder = @"nci.ilabs.inca.infoblox.com:28080/clientsock.yaws";
        serverUrlEdit.text = [[NCIWebSocketConnector interlocutor] getTapestryUrl];
        serverUrlEdit.layer.cornerRadius = 15;
        serverUrlEdit.contentVerticalAlignment = UIControlContentVerticalAlignmentCenter;
        serverUrlEdit.delegate = self;
        serverUrlEdit.returnKeyType = UIReturnKeyGo;
        [serverUrlEdit addTarget:self action:@selector(didChangeText) forControlEvents:UIControlEventEditingChanged];
        UILabel *serverEditLeftView = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, 180, editServerInputHeigth)];
        serverEditLeftView.backgroundColor = [UIColor colorWithWhite:0.99 alpha:1];
        serverEditLeftView.text = NSLocalizedString(@"Tapestry Server:   ws://", nil);
        serverEditLeftView.font = [UIFont boldSystemFontOfSize:16];
        serverEditLeftView.textColor = [UIColor blackColor];
        serverUrlEdit.leftView = serverEditLeftView;
        serverUrlEdit.leftViewMode = UITextFieldViewModeAlways;
        [self addSubview:serverUrlEdit];
        
        clearBtn = [[UIButton alloc] initWithFrame:CGRectMake(0, 0, iconDim, iconDim)];
        [clearBtn setImage:[UIImage imageNamed:@"clear_input"] forState:UIControlStateNormal];
        [clearBtn addTarget:self action:@selector(clearInput) forControlEvents:UIControlEventTouchUpInside];
        clearBtn.hidden = YES;
        serverUrlEdit.rightView = clearBtn;
        serverUrlEdit.rightViewMode =  UITextFieldViewModeAlways;
        
        goBtn = [[UIButton alloc] initWithFrame:CGRectZero];
        [goBtn setImage:[UIImage imageNamed:@"go"] forState:UIControlStateNormal];
        [goBtn  addTarget:self action:@selector(connectUrl) forControlEvents:UIControlEventTouchUpInside];
        goBtn.hidden = YES;
        [self addSubview:goBtn];
        
        self.backgroundColor = [UIColor clearColor];
        UITapGestureRecognizer *freeTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(cancelUrlChanges)];
        self.userInteractionEnabled = YES;
        freeTap.numberOfTapsRequired = 1;
        freeTap.delegate = self;
        [self addGestureRecognizer:freeTap];
    }
    return self;
}

- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldReceiveTouch:(UITouch *)touch{
    if ([touch.view isDescendantOfView:bookmarksTable]) {
        return NO;
    }
    return YES;
}


- (BOOL)textFieldShouldBeginEditing:(UITextField *)textField{
    self.active = YES;
    self.frame = CGRectMake(self.frame.origin.x, self.frame.origin.y, self.bounds.size.width, self.superview.bounds.size.height);
    self.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.1];
    [[NSNotificationCenter defaultCenter] postNotificationName:@"freeTap" object:self];
    textField.backgroundColor = [UIColor whiteColor];
    [self didChangeText];
    goBtn.hidden = NO;
    
    long rowCount = [NCIWebSocketConnector interlocutor].tapestryURLs.count + 1;
    if (rowCount > bookmarkVisibleRowsCount){
        rowCount = bookmarkVisibleRowsCount;
    }
    bookmarksTable.frame = CGRectMake(leftIndent,
                                      editServerInputHeigth,
                                      self.bounds.size.width - rightIndent,
                                      rowCount *bookmarkRowHeigth);
    
    return YES;
}

- (void)textFieldDidEndEditing:(UITextField *)textField{
    textField.backgroundColor =  [UIColor colorWithWhite:0.99 alpha:1];
    clearBtn.hidden = YES;
}

- (BOOL)textFieldShouldReturn:(UITextField *)textField
{
    [self resignFirstResponder];
    [self connectUrl];
    return YES;
}

- (void)didChangeText{
    if (serverUrlEdit.text.length == 0){
        clearBtn.hidden = YES;
    } else {
        clearBtn.hidden = NO;
    }
}

- (void)clearInput{
    serverUrlEdit.text = @"";
    clearBtn.hidden = YES;
}

-(void)connectUrl{
    goBtn.hidden = YES;
    [[NCIWebSocketConnector interlocutor] newTapestryUrl:serverUrlEdit.text];
    [bookmarksTable reloadData];
    [[NCIWebSocketConnector interlocutor] resetData];
    [self resignFirstResponder];
}

-(void)cancelUrlChanges{
    goBtn.hidden = YES;
    serverUrlEdit.text = [[NCIWebSocketConnector interlocutor] getTapestryUrl];
    [self resignFirstResponder];
}

-(BOOL)resignFirstResponder{
    self.active = NO;
    [serverUrlEdit resignFirstResponder];
    self.backgroundColor = [UIColor clearColor];
    self.frame = CGRectMake(self.frame.origin.x, self.frame.origin.y, self.bounds.size.width, editServerInputHeigth + topIndent);
    bookmarksTable.frame = CGRectMake(leftIndent,
                                      bookmarksTable.frame.origin.y,
                                      self.bounds.size.width - rightIndent, 0);
    return [super resignFirstResponder];
}

- (void)layoutSubviews {
    goBtn.frame = CGRectMake( self.bounds.size.width - 70, topIndent, iconDim, iconDim);
    serverUrlEdit.frame = CGRectMake(leftIndent, topIndent, self.bounds.size.width - rightIndent, editServerInputHeigth);
    bookmarksTable.frame = CGRectMake(leftIndent,
                                      editServerInputHeigth + topIndent + 2,
                                      self.bounds.size.width - rightIndent,
                                      bookmarksTable.frame.size.height);
}

#pragma table view data source

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section{
    return [NCIWebSocketConnector interlocutor].tapestryURLs.count + 1;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath{
    return bookmarkRowHeigth;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath{
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"BookMarkCell"];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"BookMarkCell"];
        cell.selectionStyle = UITableViewCellSelectionStyleGray;
        cell.textLabel.font = [UIFont systemFontOfSize:14];
        UIButton *accessory = [[UIButton alloc] initWithFrame:CGRectMake(0, 0, 40, 40)];
        [accessory setImage:[UIImage imageNamed:@"delete_icon"] forState:UIControlStateNormal];
        [accessory addTarget:self action:@selector(checkButtonTapped:event:) forControlEvents:UIControlEventTouchUpInside];
        cell.accessoryView = accessory;
        [cell setAccessoryType:UITableViewCellAccessoryDetailDisclosureButton];
    }
    [cell.accessoryView setHidden:(indexPath.row == 0)];
    if (indexPath.row == 0){
        cell.textLabel.text = demoUrl;
    } else {
        cell.textLabel.text = [NCIWebSocketConnector interlocutor].tapestryURLs[indexPath.row - 1];
    }

    return cell;
}

- (void)checkButtonTapped:(id)sender event:(id)event{
    NSSet *touches = [event allTouches];
    UITouch *touch = [touches anyObject];
    CGPoint currentTouchPosition = [touch locationInView:bookmarksTable];
    NSIndexPath *indexPath = [bookmarksTable indexPathForRowAtPoint: currentTouchPosition];
    if (indexPath != nil){
        [self tableView: bookmarksTable accessoryButtonTappedForRowWithIndexPath: indexPath];
    }
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath{
    if (indexPath.row != 0){
        serverUrlEdit.text = [NCIWebSocketConnector interlocutor].tapestryURLs[indexPath.row - 1];
    } else {
        serverUrlEdit.text = demoUrl;
    }
    [self connectUrl];
}

- (void)tableView:(UITableView *)tableView accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath{
    if (indexPath.row != 0){
        [[NCIWebSocketConnector interlocutor] removeURLAtIndex:indexPath.row - 1];
        [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationTop];
    }
}

@end
