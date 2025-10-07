'use client';

import React, { useState } from 'react';
import { UserListComponent } from '@/components/user/UserListComponent';
import { UpdateUserForm } from '@/components/user/UpdateUserForm';
import { DeleteUserForm } from '@/components/user/DeleteUserForm';
import { useRouter } from 'next/navigation';

type UserPageState = 'list' | 'update' | 'delete';

export default function UsersPage() {
  const router = useRouter();
  const [currentState, setCurrentState] = useState<UserPageState>('list');
  const [selectedUserId, setSelectedUserId] = useState<string>('');

  // Handle user selection from list (COUSR00C business logic)
  const handleUserSelect = (userId: string, action: 'U' | 'D') => {
    setSelectedUserId(userId);
    if (action === 'U') {
      setCurrentState('update');
    } else if (action === 'D') {
      setCurrentState('delete');
    }
  };

  // Handle exit from user list
  const handleUserListExit = () => {
    router.push('/');
  };

  // Handle user updated successfully
  const handleUserUpdated = (userId: string) => {
    console.log(`User ${userId} updated successfully`);
    // Return to list after update
    setCurrentState('list');
    setSelectedUserId('');
  };

  // Handle update user exit
  const handleUpdateUserExit = () => {
    setCurrentState('list');
    setSelectedUserId('');
  };

  // Handle user deleted successfully
  const handleUserDeleted = (userId: string) => {
    console.log(`User ${userId} deleted successfully`);
    // Return to list after delete
    setCurrentState('list');
    setSelectedUserId('');
  };

  // Handle delete user exit
  const handleDeleteUserExit = () => {
    setCurrentState('list');
    setSelectedUserId('');
  };

  // Render current screen based on state
  const renderCurrentScreen = () => {
    switch (currentState) {
      case 'list':
        return (
          <UserListComponent
            onUserSelect={handleUserSelect}
            onExit={handleUserListExit}
          />
        );

      case 'update':
        return (
          <UpdateUserForm
            selectedUserId={selectedUserId}
            onUserUpdated={handleUserUpdated}
            onExit={handleUpdateUserExit}
          />
        );

      case 'delete':
        return (
          <DeleteUserForm
            selectedUserId={selectedUserId}
            onUserDeleted={handleUserDeleted}
            onExit={handleDeleteUserExit}
          />
        );

      default:
        return (
          <div className="min-h-screen bg-gray-900 text-red-400 font-mono flex items-center justify-center">
            <div className="text-center">
              <h1 className="text-2xl font-bold mb-4">System Error</h1>
              <p className="text-lg mb-8">Invalid page state</p>
              <button
                onClick={() => router.push('/')}
                className="bg-red-700 hover:bg-red-600 text-white px-6 py-2 rounded"
              >
                Return to Home
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