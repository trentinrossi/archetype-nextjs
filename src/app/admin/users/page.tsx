'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { UserListScreen } from '@/components/UserListScreen';

export default function UserListPage() {
  const router = useRouter();

  // COUSR00C business rule: Handle user selection for update/delete
  const handleUserSelect = (userId: string, action: 'U' | 'D') => {
    // Store selected user ID for the next screen
    sessionStorage.setItem('selectedUserId', userId);
    
    if (action === 'U') {
      router.push('/admin/users/update');
    } else if (action === 'D') {
      router.push('/admin/users/delete');
    }
  };

  // Handle exit (PF3) - return to admin menu
  const handleExit = () => {
    router.push('/admin');
  };

  return (
    <UserListScreen
      onUserSelect={handleUserSelect}
      onExit={handleExit}
    />
  );
}