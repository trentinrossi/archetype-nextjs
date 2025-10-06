'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { User, APIResponse } from '@/types/user';
import { userService } from '@/services/userService';
import { Button } from '@/components/ui/Button';
import UserListManagement from '@/components/UserListManagement';
import UserAdd from '@/components/UserAdd';
import UserUpdate from '@/components/UserUpdate';
import UserDelete from '@/components/UserDelete';

type UserManagementView = 'main' | 'list' | 'add' | 'update' | 'delete';

interface UserManagementState {
  currentView: UserManagementView;
  selectedUser: User | null;
  users: User[];
  loading: boolean;
  error: string;
  successMessage: string;
  isAuthenticated: boolean;
  currentUser: any;
}

export default function UserManagementPage() {
  const [state, setState] = useState<UserManagementState>({
    currentView: 'main',
    selectedUser: null,
    users: [],
    loading: false,
    error: '',
    successMessage: '',
    isAuthenticated: false,
    currentUser: null,
  });

  const updateState = useCallback((updates: Partial<UserManagementState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const clearMessages = useCallback(() => {
    updateState({ error: '', successMessage: '' });
  }, [updateState]);

  const checkAuthentication = useCallback(async () => {
    try {
      const response = await userService.validateSession();
      if (response.success && response.data) {
        updateState({
          isAuthenticated: true,
          currentUser: response.data,
        });
      } else {
        updateState({
          isAuthenticated: false,
          currentUser: null,
          error: 'Authentication required. Please sign in.',
        });
      }
    } catch (error) {
      updateState({
        isAuthenticated: false,
        currentUser: null,
        error: 'Failed to validate authentication.',
      });
    }
  }, [updateState]);

  const handleViewChange = useCallback((view: UserManagementView, user?: User) => {
    clearMessages();
    updateState({
      currentView: view,
      selectedUser: user || null,
    });
  }, [clearMessages, updateState]);

  const handleUserListNavigation = useCallback(() => {
    handleViewChange('list');
  }, [handleViewChange]);

  const handleAddUserNavigation = useCallback(() => {
    handleViewChange('add');
  }, [handleViewChange]);

  const handleUpdateUserNavigation = useCallback(() => {
    handleViewChange('update');
  }, [handleViewChange]);

  const handleDeleteUserNavigation = useCallback(() => {
    handleViewChange('delete');
  }, [handleViewChange]);

  const handleBackToMain = useCallback(() => {
    handleViewChange('main');
  }, [handleViewChange]);

  const handleUserSelect = useCallback((user: User, action: 'update' | 'delete') => {
    if (action === 'update') {
      handleViewChange('update', user);
    } else if (action === 'delete') {
      handleViewChange('delete', user);
    }
  }, [handleViewChange]);

  const handleUserCreated = useCallback((user: User) => {
    updateState({
      successMessage: `User ${user.userId} has been successfully created!`,
      users: [...state.users, user],
    });
  }, [state.users, updateState]);

  const handleUserUpdated = useCallback((user: User) => {
    updateState({
      successMessage: `User ${user.userId} has been successfully updated!`,
      users: state.users.map(u => u.userId === user.userId ? user : u),
    });
  }, [state.users, updateState]);

  const handleUserDeleted = useCallback((userId: string) => {
    updateState({
      successMessage: `User ${userId} has been successfully deleted!`,
      users: state.users.filter(user => user.userId !== userId),
    });
    
    // Auto-return to main menu after successful deletion
    setTimeout(() => {
      handleViewChange('main');
    }, 2000);
  }, [state.users, updateState, handleViewChange]);

  const handleSignOut = useCallback(async () => {
    try {
      await userService.signout();
      updateState({
        isAuthenticated: false,
        currentUser: null,
        currentView: 'main',
        successMessage: 'Successfully signed out.',
      });
    } catch (error) {
      updateState({
        error: 'Failed to sign out properly.',
      });
    }
  }, [updateState]);

  const handleExit = useCallback(() => {
    if (typeof window !== 'undefined') {
      const confirmExit = window.confirm('Are you sure you want to exit the User Management system?');
      if (confirmExit) {
        handleSignOut();
        window.location.href = '/';
      }
    }
  }, [handleSignOut]);

  useEffect(() => {
    checkAuthentication();
  }, [checkAuthentication]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case 'F1':
          event.preventDefault();
          if (state.currentView === 'main') {
            handleUserListNavigation();
          }
          break;
        case 'F2':
          event.preventDefault();
          if (state.currentView === 'main') {
            handleAddUserNavigation();
          }
          break;
        case 'F3':
          event.preventDefault();
          if (state.currentView === 'main') {
            handleExit();
          } else {
            handleBackToMain();
          }
          break;
        case 'F4':
          event.preventDefault();
          if (state.currentView === 'main') {
            handleUpdateUserNavigation();
          }
          break;
        case 'F5':
          event.preventDefault();
          if (state.currentView === 'main') {
            handleDeleteUserNavigation();
          }
          break;
        case 'F12':
        case 'Escape':
          event.preventDefault();
          if (state.currentView !== 'main') {
            handleBackToMain();
          } else {
            handleExit();
          }
          break;
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [
    state.currentView,
    handleUserListNavigation,
    handleAddUserNavigation,
    handleUpdateUserNavigation,
    handleDeleteUserNavigation,
    handleBackToMain,
    handleExit,
  ]);

  const renderMainMenu = () => (
    <div className="bg-background border border-border rounded-lg p-8 max-w-4xl mx-auto">
      {/* Header */}
      <div className="mb-8 text-center">
        <h1 className="text-3xl font-bold text-foreground mb-4">User Management System</h1>
        <p className="text-lg text-muted-foreground mb-2">COUSR00C - Main Menu</p>
        {state.currentUser && (
          <div className="text-sm text-muted-foreground">
            <p>Welcome, {state.currentUser.firstName} {state.currentUser.lastName}</p>
            <p>User ID: <span className="font-mono">{state.currentUser.userId}</span> | 
               Type: {state.currentUser.userType === 'A' ? 'Administrator' : 'General User'}</p>
          </div>
        )}
      </div>

      {/* Success Message */}
      {state.successMessage && (
        <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-md">
          <p className="text-green-800 text-sm font-medium">{state.successMessage}</p>
        </div>
      )}

      {/* Error Message */}
      {state.error && (
        <div className="mb-6 p-4 bg-destructive/10 border border-destructive/20 rounded-md">
          <p className="text-destructive text-sm">{state.error}</p>
        </div>
      )}

      {/* Authentication Check */}
      {!state.isAuthenticated ? (
        <div className="text-center py-8">
          <div className="mb-6 p-4 bg-orange-50 border border-orange-200 rounded-md">
            <p className="text-orange-800 font-medium">Authentication Required</p>
            <p className="text-orange-700 text-sm mt-2">
              Please sign in to access the User Management system.
            </p>
          </div>
          <Button
            onClick={() => window.location.href = '/login'}
            variant="primary"
            size="lg"
          >
            Go to Sign In
          </Button>
        </div>
      ) : (
        <>
          {/* Menu Options */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
            {/* User List */}
            <div className="p-6 bg-muted/30 border border-border rounded-lg hover:bg-muted/50 transition-colors">
              <h3 className="text-xl font-semibold text-foreground mb-3">
                <span className="text-primary">F1</span> - User List
              </h3>
              <p className="text-muted-foreground mb-4">
                View and browse all users in the system. Search, filter, and manage user records.
              </p>
              <Button
                onClick={handleUserListNavigation}
                variant="primary"
                className="w-full"
              >
                View User List (COUSR00C)
              </Button>
            </div>

            {/* Add User */}
            <div className="p-6 bg-muted/30 border border-border rounded-lg hover:bg-muted/50 transition-colors">
              <h3 className="text-xl font-semibold text-foreground mb-3">
                <span className="text-primary">F2</span> - Add User
              </h3>
              <p className="text-muted-foreground mb-4">
                Create new user accounts with required information and access permissions.
              </p>
              <Button
                onClick={handleAddUserNavigation}
                variant="primary"
                className="w-full"
              >
                Add New User (COUSR01C)
              </Button>
            </div>

            {/* Update User */}
            <div className="p-6 bg-muted/30 border border-border rounded-lg hover:bg-muted/50 transition-colors">
              <h3 className="text-xl font-semibold text-foreground mb-3">
                <span className="text-primary">F4</span> - Update User
              </h3>
              <p className="text-muted-foreground mb-4">
                Modify existing user information, passwords, and access permissions.
              </p>
              <Button
                onClick={handleUpdateUserNavigation}
                variant="primary"
                className="w-full"
              >
                Update User (COUSR02C)
              </Button>
            </div>

            {/* Delete User */}
            <div className="p-6 bg-muted/30 border border-border rounded-lg hover:bg-muted/50 transition-colors">
              <h3 className="text-xl font-semibold text-foreground mb-3">
                <span className="text-destructive">F5</span> - Delete User
              </h3>
              <p className="text-muted-foreground mb-4">
                Remove user accounts from the system. This action cannot be undone.
              </p>
              <Button
                onClick={handleDeleteUserNavigation}
                variant="destructive"
                className="w-full"
              >
                Delete User (COUSR03C)
              </Button>
            </div>
          </div>

          {/* System Information */}
          <div className="mb-6 p-4 bg-blue-50 border border-blue-200 rounded-md">
            <h4 className="font-semibold text-blue-900 mb-2">System Information</h4>
            <div className="text-sm text-blue-800 space-y-1">
              <p>• User Management System v1.0</p>
              <p>• All user operations are logged and audited</p>
              <p>• Administrator privileges required for user management</p>
              <p>• Session timeout: 24 hours</p>
            </div>
          </div>

          {/* Action Buttons */}
          <div className="flex justify-between items-center pt-6 border-t border-border">
            <Button
              onClick={handleSignOut}
              variant="outline"
            >
              Sign Out
            </Button>
            <Button
              onClick={handleExit}
              variant="outline"
            >
              Exit System (F3)
            </Button>
          </div>
        </>
      )}

      {/* Keyboard Shortcuts Help */}
      <div className="mt-6 pt-4 border-t border-border">
        <p className="text-xs text-muted-foreground text-center">
          <strong>Keyboard Shortcuts:</strong> F1 = User List | F2 = Add User | F3 = Exit | F4 = Update User | F5 = Delete User | ESC = Exit
        </p>
      </div>
    </div>
  );

  const renderCurrentView = () => {
    switch (state.currentView) {
      case 'list':
        return (
          <UserListManagement
            onUserSelect={handleUserSelect}
            onExit={handleBackToMain}
          />
        );

      case 'add':
        return (
          <UserAdd
            onUserCreated={handleUserCreated}
            onExit={handleBackToMain}
          />
        );

      case 'update':
        return (
          <UserUpdate
            onUserUpdated={handleUserUpdated}
            onExit={handleBackToMain}
          />
        );

      case 'delete':
        return (
          <UserDelete
            onUserDeleted={handleUserDeleted}
            onExit={handleBackToMain}
          />
        );

      default:
        return renderMainMenu();
    }
  };

  return (
    <div className="min-h-screen bg-muted/20 py-8 px-4">
      <div className="container mx-auto">
        {renderCurrentView()}
      </div>
    </div>
  );
}