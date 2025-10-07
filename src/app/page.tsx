'use client';

import React, { useState } from 'react';
import { SignonForm } from '@/components/user/SignonForm';
import { AdminMenu } from '@/components/user/AdminMenu';
import { UserListComponent } from '@/components/user/UserListComponent';
import { AddUserForm } from '@/components/user/AddUserForm';
import { UpdateUserForm } from '@/components/user/UpdateUserForm';
import { DeleteUserForm } from '@/components/user/DeleteUserForm';
import { SignonResponseDTO } from '@/types/user';

type AppState = 'signon' | 'admin-menu' | 'user-list' | 'add-user' | 'update-user' | 'delete-user' | 'exit';

export default function HomePage() {
  const [currentState, setCurrentState] = useState<AppState>('signon');
  const [userType, setUserType] = useState<'ADMIN' | 'GENERAL'>('GENERAL');
  const [selectedUserId, setSelectedUserId] = useState<string>('');
  const [exitMessage, setExitMessage] = useState('');

  // Handle successful signon (COSGN00C business logic)
  const handleSignonSuccess = (response: SignonResponseDTO) => {
    setUserType(response.userType);
    
    // Redirect based on user type (COBOL business logic)
    if (response.redirectProgram === 'COADM01C') {
      setCurrentState('admin-menu');
    } else {
      // For GENERAL users, show limited menu or redirect to general menu
      setCurrentState('admin-menu');
    }
  };

  // Handle signon exit (PF3 key)
  const handleSignonExit = () => {
    setCurrentState('exit');
    setExitMessage('Thank you for using CardDemo Application');
  };

  // Handle invalid key press
  const handleInvalidKey = () => {
    // Stay on signon screen with error message
    console.log('Invalid key pressed');
  };

  // Handle admin menu selection
  const handleMenuSelect = (option: 'list' | 'add' | 'update' | 'delete' | 'exit') => {
    switch (option) {
      case 'list':
        setCurrentState('user-list');
        setSelectedUserId('');
        break;
      case 'add':
        setCurrentState('add-user');
        break;
      case 'update':
        setCurrentState('update-user');
        setSelectedUserId('');
        break;
      case 'delete':
        setCurrentState('delete-user');
        setSelectedUserId('');
        break;
      case 'exit':
        setCurrentState('signon');
        setUserType('GENERAL');
        setSelectedUserId('');
        break;
    }
  };

  // Handle user selection from list (COUSR00C business logic)
  const handleUserSelect = (userId: string, action: 'U' | 'D') => {
    setSelectedUserId(userId);
    if (action === 'U') {
      setCurrentState('update-user');
    } else if (action === 'D') {
      setCurrentState('delete-user');
    }
  };

  // Handle user list exit (PF3 key)
  const handleUserListExit = () => {
    setCurrentState('admin-menu');
  };

  // Handle user added successfully
  const handleUserAdded = (userId: string) => {
    console.log(`User ${userId} added successfully`);
    // Stay on add user form for additional entries (COBOL business logic)
  };

  // Handle add user exit
  const handleAddUserExit = () => {
    setCurrentState('admin-menu');
  };

  // Handle user updated successfully
  const handleUserUpdated = (userId: string) => {
    console.log(`User ${userId} updated successfully`);
    // Stay on update form (COBOL business logic)
  };

  // Handle update user exit
  const handleUpdateUserExit = () => {
    setCurrentState('admin-menu');
  };

  // Handle user deleted successfully
  const handleUserDeleted = (userId: string) => {
    console.log(`User ${userId} deleted successfully`);
    // Stay on delete form for additional deletions (COBOL business logic)
  };

  // Handle delete user exit
  const handleDeleteUserExit = () => {
    setCurrentState('admin-menu');
  };

  // Render current screen based on state
  const renderCurrentScreen = () => {
    switch (currentState) {
      case 'signon':
        return (
          <SignonForm
            onSignonSuccess={handleSignonSuccess}
            onExit={handleSignonExit}
            onInvalidKey={handleInvalidKey}
          />
        );

      case 'admin-menu':
        return (
          <AdminMenu
            onMenuSelect={handleMenuSelect}
            userType={userType}
          />
        );

      case 'user-list':
        return (
          <UserListComponent
            onUserSelect={handleUserSelect}
            onExit={handleUserListExit}
          />
        );

      case 'add-user':
        return (
          <AddUserForm
            onUserAdded={handleUserAdded}
            onExit={handleAddUserExit}
          />
        );

      case 'update-user':
        return (
          <UpdateUserForm
            selectedUserId={selectedUserId}
            onUserUpdated={handleUserUpdated}
            onExit={handleUpdateUserExit}
          />
        );

      case 'delete-user':
        return (
          <DeleteUserForm
            selectedUserId={selectedUserId}
            onUserDeleted={handleUserDeleted}
            onExit={handleDeleteUserExit}
          />
        );

      case 'exit':
        return (
          <div className="min-h-screen bg-gray-900 text-green-400 font-mono flex items-center justify-center">
            <div className="text-center">
              <h1 className="text-2xl font-bold mb-4">CARDDEMO</h1>
              <p className="text-lg mb-8">{exitMessage}</p>
              <button
                onClick={() => setCurrentState('signon')}
                className="bg-green-700 hover:bg-green-600 text-white px-6 py-2 rounded"
              >
                Return to Sign On
              </button>
            </div>
          </div>
        );

      default:
        return (
          <div className="min-h-screen bg-gray-900 text-red-400 font-mono flex items-center justify-center">
            <div className="text-center">
              <h1 className="text-2xl font-bold mb-4">System Error</h1>
              <p className="text-lg mb-8">Invalid application state</p>
              <button
                onClick={() => setCurrentState('signon')}
                className="bg-red-700 hover:bg-red-600 text-white px-6 py-2 rounded"
              >
                Return to Sign On
              </button>
            </div>
          </div>
        );
    }
  };

  return (
    <div className="w-full">
      {renderCurrentScreen()}
    </div>
  );
}